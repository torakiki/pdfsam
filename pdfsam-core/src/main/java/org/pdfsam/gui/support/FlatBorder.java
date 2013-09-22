/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 22/set/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.gui.support;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Insets;

import javax.swing.border.Border;

import static org.pdfsam.gui.view.Views.SMALL_GAP;

/**
 * A simplified and reusable version of the {@link bibliothek.extension.gui.dock.theme.flat.FlatBorder} found in DockingFrames
 * 
 * @author Andrea Vacondio
 */
public class FlatBorder implements Border {
    private static final float FACTOR = 0.85f;

    @Override
    public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {

        Color background = c.getBackground();
        Color light = darker(background);
        Color middle = darker(light);
        Color dark = darker(middle);

        g.setColor(dark);
        g.drawRect(x, y, width - SMALL_GAP, height - SMALL_GAP);

        g.setColor(middle);
        g.drawLine(x + width - 2, y + 1, x + width - 2, y + height - SMALL_GAP);
        g.drawLine(x + width - SMALL_GAP, y, x + width - SMALL_GAP, y);
        g.drawLine(x, y + height - SMALL_GAP, x, y + height - SMALL_GAP);
        g.drawLine(x + 1, y + height - 2, x + width - SMALL_GAP, y + height - 2);

        g.setColor(light);
        g.drawLine(x + width - 1, y + 1, x + width - 1, y + height - 2);
        g.drawLine(x + width - 2, y, x + width - 2, y);
        g.drawLine(x, y + height - 2, x, y + height - 2);
        g.drawLine(x + 1, y + height - 1, x + width - 2, y + height - 1);
        g.drawLine(x + width - 2, y + height - 2, x + width - 2, y + height - 2);

    }

    /**
     * Creates a darker version of <code>c</code>.
     * 
     * @param c
     *            the original color
     * @return a darker version of the color
     */
    private Color darker(Color c) {
        return new Color(Math.max((int) (c.getRed() * FACTOR), 0), Math.max((int) (c.getGreen() * FACTOR), 0),
                Math.max((int) (c.getBlue() * FACTOR), 0));
    }

    public Insets getBorderInsets(Component c) {
        return new Insets(1, 1, SMALL_GAP, SMALL_GAP);
    }

    public boolean isBorderOpaque() {
        return false;
    }

}