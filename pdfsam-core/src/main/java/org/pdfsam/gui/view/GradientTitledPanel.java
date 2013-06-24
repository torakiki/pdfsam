/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 05/feb/2013
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
package org.pdfsam.gui.view;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.FlowLayout;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.pdfsam.gui.support.ToolTipBuilder;
import org.pdfsam.gui.view.GradientPanel.GradientOrientation;

import static org.apache.commons.lang3.StringUtils.isNotBlank;

/**
 * A panel using a gradient to display the title. This panel uses a {@link BorderLayout} where PAGE_START is used for the title and typically another panel is added as CENTER.
 * 
 * @author Andrea Vacondio
 * 
 */
public class GradientTitledPanel extends JPanel {

    public GradientTitledPanel(String title, ToolTipBuilder tooltip, GradientOrientation orientation) {
        BorderLayout layout = new BorderLayout();
        layout.setVgap(Views.GAP);
        setLayout(layout);
        add(buildTitlePanel(title, tooltip.toString(), orientation), BorderLayout.PAGE_START);
    }

    private Component buildTitlePanel(String title, String tooltip, GradientOrientation orientation) {
        GradientPanel titlePanel = new GradientPanel(orientation);
        JLabel titleLabel = new JLabel(title);
        titlePanel.setLayout(new FlowLayout(FlowLayout.LEADING, Views.GAP, Views.SMALL_GAP));
        if (isNotBlank(tooltip)) {
            JLabel helpIcon = new JLabel(new ImageIcon(GradientTitledPanel.class.getResource("/images/help.png")));
            helpIcon.setToolTipText(tooltip);
            titlePanel.add(helpIcon);
        }
        titlePanel.add(titleLabel);
        return titlePanel;
    }

}
