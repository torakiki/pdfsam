/*
 * Created on 08/feb/2013
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
package org.pdfsam.gui.about;

import java.awt.Color;
import java.awt.Dimension;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.pdfsam.configuration.PdfsamProperties;

/**
 * Panel displaying the PDFsam image in the about panel.
 * 
 * @author Andrea Vacondio
 * 
 */
class AboutImagePanel extends JPanel {

    AboutImagePanel() {
        JLabel image = new JLabel(new ImageIcon(AboutDialog.class.getResource("/images/pdfsam_"
                + PdfsamProperties.PACKAGE + "_128.png")));
        image.setMinimumSize(new Dimension(128, 128));
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        Dimension fillerSize = new Dimension(10, 0);
        add(new Box.Filler(fillerSize, fillerSize, fillerSize));
        add(Box.createVerticalGlue());
        setBackground(Color.WHITE);
        add(image);
        add(Box.createVerticalGlue());
    }

}
