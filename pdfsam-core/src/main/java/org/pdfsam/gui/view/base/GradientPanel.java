/*
 * Created on 05/feb/2013
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
package org.pdfsam.gui.view.base;

import java.awt.Color;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;

import javax.swing.JPanel;
import javax.swing.UIManager;

import static org.pdfsam.support.RequireUtils.require;

/**
 * Panel with a gradient background.
 * 
 * @author Andrea Vacondio
 * 
 */
public class GradientPanel extends JPanel {
    private GradientOrientation orientation = GradientOrientation.HORIZONTAL;
    private Color startColor = UIManager.getColor("MenuItem.selectionBackground");
    private Color endColor = UIManager.getColor("MenuItem.background");

    /**
     * Constructor using default colors
     */
    public GradientPanel(GradientOrientation orientation) {
        require(orientation != null, "Orientation cannot be null");
        this.orientation = orientation;
        // setBorder(BorderFactory.createLineBorder(startColor, 1));
    }

    public GradientPanel(GradientOrientation orientation, Color startColor, Color endColor) {
        this(orientation);
        require(startColor != null && endColor != null, "Gradient colors cannot be null");
        this.startColor = startColor;
        this.endColor = endColor;
        // setBorder(BorderFactory.createLineBorder(startColor, 1));
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);

        GradientPaint gradientPaint = getGradient();
        if (g instanceof Graphics2D) {
            Graphics2D graphics2D = (Graphics2D) g;
            graphics2D.setPaint(gradientPaint);
            graphics2D.fillRect(0, 0, getWidth(), getHeight());
        }
    }

    private GradientPaint getGradient() {
        int panelWidth = getWidth();
        int panelHeight = getHeight();
        if (GradientOrientation.VERTICAL == orientation) {
            float w = panelWidth / 2.0f;
            return new GradientPaint(w, 0, startColor, w, panelHeight, endColor, false);
        }
        float h = panelHeight / 2.0f;
        return new GradientPaint(0, h, startColor, panelWidth, h, endColor, false);
    }

    /**
     * Orientation for the gradient backgroud
     * 
     * @author Andrea Vacondio
     * 
     */
    public enum GradientOrientation {
        VERTICAL,
        HORIZONTAL;
    }
}
