package org.colomoto.biolqm;

import java.util.HashMap;
import java.util.Map;

/**
 * Simple storage for layout information
 *
 * @author Aurelien Naldi
 */
public class ModelLayout {

    private final Map<NodeInfo, LayoutInfo> layout = new HashMap<>();

    public LayoutInfo getInfo(NodeInfo ni) {
        return this.layout.get(ni);
    }

    public void crop() {
        this.crop(5);
    }

    public void crop(int margin) {
        this.crop(margin, margin);
    }

    public void crop(int marginx, int marginy) {

        // Find the min values
        int minx = Integer.MAX_VALUE;
        int miny = Integer.MAX_VALUE;
        for (LayoutInfo info: this.layout.values()) {
            if (info.x < minx) {
                minx = info.x;
            }
            if (info.y < miny) {
                miny = info.y;
            }
        }

        // Apply the margin
        minx -= marginx;
        miny -= marginy;

        // Shift every position
        for (LayoutInfo info: this.layout.values()) {
            info.x = info.x - minx;
            info.y = info.y - miny;
        }
    }

    public void scale(float scale) {
        this.scale(scale, scale);
    }

    public void scale(float scalex, float scaley) {
        for (LayoutInfo info: this.layout.values()) {
            info.x *= scalex;
            info.y *= scaley;
        }
    }

    public LayoutInfo setPosition(NodeInfo ni, int x, int y) {
        LayoutInfo li = layout.get(ni);
        if (li == null) {
            li = new LayoutInfo(x,y);
            layout.put(ni, li);
            return li;
        }

        li.x = x;
        li.y = y;
        return li;
    }

    public void set(NodeInfo ni, int x, int y, int width, int height) {
        LayoutInfo li = layout.get(ni);
        if (li == null) {
            li = new LayoutInfo(x,y);
            layout.put(ni, li);
            return;
        }

        li.x = x;
        li.y = y;
    }

    public void copy(NodeInfo ni, LayoutInfo source) {
        if (source == null) {
            layout.remove(ni);
            return;
        }
        this.set(ni, source.x, source.y, source.width, source.height);
    }

    public class LayoutInfo {
        public int x,y;
        public int width, height;

        protected LayoutInfo(int x, int y, int width, int height) {
            this.x = x;
            this.y = y;
            this.width = width;
            this.height = height;
        }

        protected LayoutInfo(int x, int y) {
            this(x,y, 0, 0);
        }
    }

}

