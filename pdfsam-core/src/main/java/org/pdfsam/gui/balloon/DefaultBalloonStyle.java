/*
 * Created on 13/apr/2012
 * Copyright 2010 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation; 
 * either version 2 of the License.
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with this program; 
 * if not, write to the Free Software Foundation, Inc., 
 *  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package org.pdfsam.gui.balloon;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.Paint;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.TexturePaint;
import java.awt.geom.GeneralPath;
import java.awt.image.BufferedImage;
import java.io.IOException;

import javax.imageio.ImageIO;

import net.java.balloontip.styles.BalloonTipStyle;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Default balloon style for the application
 * 
 * @author Andrea Vacondio
 * 
 */
class DefaultBalloonStyle extends BalloonTipStyle {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultBalloonStyle.class);
    private static final int CORNER_ARCH = 7;
    private static Paint background = Color.BLACK;

    static {
        BufferedImage bg;
        try {
            bg = ImageIO.read(DefaultBalloonStyle.class.getResource("/images/bg_tooltip.png"));
            Rectangle bgBounds = new Rectangle(0, 0, bg.getWidth(), bg.getHeight());
            background = new TexturePaint(bg, bgBounds);
        } catch (IOException e) {
            LOG.warn("Unable to load tooltip background image");
        }
    }

    public DefaultBalloonStyle() {
        horizontalOffset = CORNER_ARCH;
    }

    @Override
    public Insets getBorderInsets(Component c) {
        if (flipY) {
            return new Insets(CORNER_ARCH + verticalOffset, CORNER_ARCH, CORNER_ARCH, CORNER_ARCH);
        }
        return new Insets(CORNER_ARCH, CORNER_ARCH, CORNER_ARCH + verticalOffset, CORNER_ARCH);
    }

    @Override
    public boolean isBorderOpaque() {
        return false;
    }

    public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {

        Graphics2D g2d = (Graphics2D) g;
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        int yTop; // Y-coordinate of the top side of the balloon
        int yBottom; // Y-coordinate of the bottom side of the balloon
        if (flipY) {
            yTop = y + verticalOffset;
            yBottom = y + height;
        } else {
            yTop = y;
            yBottom = y + height - verticalOffset;
        }

        // Draw the outline of the balloon
        GeneralPath outline = new GeneralPath();
        outline.moveTo(x + CORNER_ARCH, yTop);

        // Top left corner
        outline.quadTo(x, yTop, x, yTop + CORNER_ARCH);

        // Left side
        outline.lineTo(x, yBottom - CORNER_ARCH);

        // Bottom left corner
        outline.quadTo(x, yBottom, x + CORNER_ARCH, yBottom);

        paintBottomSide(x, width, yBottom, outline);

        // Bottom right corner
        outline.quadTo(x + width, yBottom, x + width, yBottom - CORNER_ARCH);

        // Right side
        outline.lineTo(x + width, yTop + CORNER_ARCH);

        // Top right corner
        outline.quadTo(x + width, yTop, x + width - CORNER_ARCH, yTop);

        paintTopSide(x, width, yTop, outline);

        outline.closePath();

        g2d.setPaint(background);
        g2d.fill(outline);

    }

    private void paintBottomSide(int x, int width, int yBottom, GeneralPath outline) {
        if (!flipX && !flipY) {
            outline.lineTo(x + horizontalOffset - verticalOffset, yBottom);
            outline.lineTo(x + horizontalOffset, yBottom + verticalOffset);
            outline.lineTo(x + horizontalOffset + verticalOffset, yBottom);
        } else if (flipX && !flipY) {
            outline.lineTo(x + width - horizontalOffset - verticalOffset, yBottom);
            outline.lineTo(x + width - horizontalOffset, yBottom + verticalOffset);
            outline.lineTo(x + width - horizontalOffset + verticalOffset, yBottom);
        }
        outline.lineTo(x + width - CORNER_ARCH, yBottom);
    }

    private void paintTopSide(int x, int width, int yTop, GeneralPath outline) {
        if (!flipX && flipY) {
            outline.lineTo(x + horizontalOffset + verticalOffset, yTop);
            outline.lineTo(x + horizontalOffset, yTop - verticalOffset);
            outline.lineTo(x + horizontalOffset - verticalOffset, yTop);
        } else if (flipX && flipY) {
            outline.lineTo(x + width - horizontalOffset + verticalOffset, yTop);
            outline.lineTo(x + width - horizontalOffset, yTop - verticalOffset);
            outline.lineTo(x + width - horizontalOffset - verticalOffset, yTop);
        }
    }

    @Override
    public int getMinimalHorizontalOffset() {
        return CORNER_ARCH + verticalOffset;
    }
}
