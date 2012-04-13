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

import javax.swing.JComponent;
import javax.swing.JLabel;

import net.java.balloontip.BalloonTip;
import net.java.balloontip.positioners.CenteredPositioner;
import net.java.balloontip.styles.BalloonTipStyle;
import net.java.balloontip.utils.ToolTipUtils;

/**
 * Utility to create balloon tooltips for components
 * 
 * @author Andrea Vacondio
 * 
 */
public final class BalloonUtils {

    private static final int INITIAL_DELAY = 1000;
    private static final int DISPLAY_DELAY = 4000;

    private BalloonUtils() {
        // hide
    }

    /**
     * Creates a balloon tooltip for the given component containing the given message
     * 
     * @param targetComponent
     * @param balloonMessage
     */
    public static void createBalloonFor(JComponent targetComponent, String balloonMessage) {
        BalloonTipStyle style = new DefaultBalloonStyle();
        JLabel label = new JLabel(balloonMessage);
        label.setForeground(Color.WHITE);
        // TODO consider to change the font
        BalloonTip myBalloonTip = new BalloonTip(targetComponent, label, style, false);
        myBalloonTip.setPositioner(new CenteredPositioner(3));
        ToolTipUtils.balloonToToolTip(myBalloonTip, INITIAL_DELAY, DISPLAY_DELAY);
    }
}
