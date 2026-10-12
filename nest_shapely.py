"""nest_shapely.py : nest real pattern shapes (curves, wedges, tapers) on fabric.

Called from R through reticulate (see shapely_nest.R). Needs: pip install shapely numpy

How it works
1. Wedge-shaped pieces that are identical (A-line panels, anarkali slices, flared legs) are paired
   head-to-toe: the second copy is turned 180 degrees and slid against the first until the two
   slanted edges touch. The pair behaves like one nearly rectangular block.
2. Every block gets a column profile at 1 cm resolution (where its top and bottom edges are).
3. The fabric keeps a skyline (how far down each column is used). A block is dropped at the x position
   and rotation where it sits lowest. Many piece orders are tried and the shortest length wins.

Cutting assumption: fabric is cut double (right sides together), so a turned copy is a valid pair.
Coordinates: x across the fabric width, y along the length (downwards), all in cm.
"""
import math
import random

import numpy as np
from numpy.lib.stride_tricks import sliding_window_view

BIG = 1e9


def _rot(deg):
    t = math.radians(deg)
    c, s = math.cos(t), math.sin(t)
    return np.array([[c, -s, 0.0], [s, c, 0.0], [0.0, 0.0, 1.0]])


def _shift(tx, ty):
    return np.array([[1.0, 0.0, tx], [0.0, 1.0, ty], [0.0, 0.0, 1.0]])


def _allowed_angles(locked, nap):
    if nap:
        return [0]                 # one-way nap or motif: every piece must point the same way
    if locked:
        return [0, 180]            # straight grain: may be turned head-to-toe, not sideways
    return [0, 90, 180, 270]


def _profiles(geom, angle, gap):
    """Top and bottom edge of a block in every 1 cm column, after rotating it by `angle`."""
    from shapely import affinity
    from shapely.geometry import LineString

    q = affinity.rotate(geom, angle, origin=(0, 0))
    p = q.buffer(gap / 2.0, join_style=2) if gap > 0 else q       # clearance between pieces
    minx, miny, maxx, maxy = p.bounds
    n = int(math.ceil(maxx - minx - 1e-9))
    top = np.full(n, BIG)
    bot = np.full(n, -BIG)
    for c in range(n):
        xc = minx + c + 0.5
        inter = p.intersection(LineString([(xc, miny - 1), (xc, maxy + 1)]))
        if not inter.is_empty:
            b = inter.bounds
            top[c] = b[1] - miny
            bot[c] = b[3] - miny
    return {"top": top, "bot": bot, "n": n, "minx": minx, "miny": miny, "height": float(bot.max())}


def _pack(order, prof, allowed, n_cols):
    """Drop blocks onto a skyline in the given order. Pure numpy, so it is fast."""
    sky = np.zeros(n_cols)
    placed = []
    for i in order:
        best = None
        for ang in allowed[i]:
            pr = prof[(i, ang)]
            n = pr["n"]
            if n > n_cols:
                continue
            windows = sliding_window_view(sky, n)                 # every x position at once
            ys = np.maximum((windows - pr["top"]).max(axis=1), 0.0)
            bottoms = ys + pr["height"]
            x = int(np.argmin(bottoms))                           # lowest bottom, then leftmost
            if best is None or bottoms[x] < best[0] - 1e-9:       # keep only if it beats the best so far
                best = (float(bottoms[x]), ang, x, float(ys[x]))
        if best is None:
            return None, None
        _, ang, x, y = best
        pr = prof[(i, ang)]
        sky[x:x + pr["n"]] = np.maximum(sky[x:x + pr["n"]], y + pr["bot"])
        placed.append((i, ang, x, y))
    return placed, float(sky.max())


def _make_pair(shp, gap, n_cols):
    """Turn a copy 180 degrees and slide it against the original. Returns (block, local matrix) or None."""
    from shapely import affinity
    from shapely.ops import unary_union

    b0 = affinity.rotate(shp, 180, origin=(0, 0))
    bminx, bminy, _, _ = b0.bounds
    ax0, ay0, ax1, _ = shp.bounds
    by = ay0 - bminy                                  # line the tops up
    for dx in np.arange(0.0, (ax1 - ax0) + 1.0, 0.5):  # slide right until the edges are `gap` apart
        bx = ax0 + dx - bminx
        b = affinity.translate(b0, bx, by)
        if (not shp.intersects(b)) and shp.distance(b) >= gap - 1e-6:
            block = unary_union([shp, b])
            bx0, _, bx1, _ = block.bounds
            if bx1 - bx0 <= n_cols:
                return block, _shift(bx, by) @ _rot(180)
            return None
    return None


def _build_items(shapes, locked, nap, gap, n_cols):
    """Group identical wedge pieces into head-to-toe pairs; everything else stays on its own."""
    items = []
    groups = {}
    for i, shp in enumerate(shapes):
        wedge = shp.area < 0.92 * (shp.bounds[2] - shp.bounds[0]) * (shp.bounds[3] - shp.bounds[1])
        if bool(locked[i]) and not nap and wedge:
            key = tuple(np.round(np.asarray(shp.exterior.coords), 1).ravel())
            groups.setdefault(key, []).append(i)
        else:
            items.append({"geom": shp, "members": [(i, np.eye(3))], "locked": bool(locked[i])})
    for members in groups.values():
        while len(members) >= 2:
            i, j = members[0], members[1]
            pair = _make_pair(shapes[i], gap, n_cols)
            if pair is None:                           # cannot pair: keep them separate
                for k in (i, j):
                    items.append({"geom": shapes[k], "members": [(k, np.eye(3))], "locked": True})
            else:
                block, m_b = pair
                items.append({"geom": block, "members": [(i, np.eye(3)), (j, m_b)], "locked": True})
            members = members[2:]
        for k in members:
            items.append({"geom": shapes[k], "members": [(k, np.eye(3))], "locked": True})
    return items


def nest_polygons(polys, names, locked, nap, width, gap, trials, seed):
    """polys: list of Nx2 arrays (cm). Returns one 2D transform per piece, plus length and used area."""
    from shapely.geometry import Polygon

    random.seed(int(seed))
    gap = float(gap)
    n_cols = int(math.floor(width))
    shapes = [Polygon(np.asarray(a, dtype=float)) for a in polys]

    for i, shp in enumerate(shapes):
        w0 = shp.bounds[2] - shp.bounds[0]
        h0 = shp.bounds[3] - shp.bounds[1]
        narrow = min(w0, h0) if not (bool(locked[i]) or nap) else w0
        if narrow + gap > width:
            raise ValueError("'%s' does not fit on %g cm wide fabric" % (names[i], width))

    items = _build_items(shapes, locked, bool(nap), gap, n_cols)
    allowed = [_allowed_angles(it["locked"], bool(nap)) for it in items]

    prof = {}
    for k, it in enumerate(items):
        for ang in allowed[k]:
            prof[(k, ang)] = _profiles(it["geom"], ang, gap)
        if all(prof[(k, ang)]["n"] > n_cols for ang in allowed[k]):
            raise ValueError("'%s' does not fit on %g cm wide fabric" % (names[it["members"][0][0]], width))

    idx = list(range(len(items)))
    orders = [sorted(idx, key=lambda k: -items[k]["geom"].area),
              sorted(idx, key=lambda k: -(items[k]["geom"].bounds[3] - items[k]["geom"].bounds[1])),
              sorted(idx, key=lambda k: -(items[k]["geom"].bounds[2] - items[k]["geom"].bounds[0]))]
    for _ in range(int(trials)):
        o = idx[:]
        random.shuffle(o)
        orders.append(o)

    best_len, best_placed = None, None
    for o in orders:                                    # loop over many orders, keep the shortest
        placed, length = _pack(o, prof, allowed, n_cols)
        if placed is not None and (best_len is None or length < best_len - 1e-9):
            best_len, best_placed = length, placed

    # Final transform of each piece: shift( rotate( local ) ), where local puts it inside its block
    out = {"names": [], "a": [], "b": [], "c": [], "d": [], "tx": [], "ty": [],
           "length": best_len - gap / 2.0, "area": float(sum(s.area for s in shapes))}
    for (k, ang, x, y) in best_placed:
        pr = prof[(k, ang)]
        block_m = _shift(x - pr["minx"], y - pr["miny"]) @ _rot(ang)
        for (i, m_local) in items[k]["members"]:
            m = block_m @ m_local
            out["names"].append(str(names[i]))
            out["a"].append(float(m[0, 0])); out["b"].append(float(m[0, 1]))
            out["c"].append(float(m[1, 0])); out["d"].append(float(m[1, 1]))
            out["tx"].append(float(m[0, 2])); out["ty"].append(float(m[1, 2]))
    return out
