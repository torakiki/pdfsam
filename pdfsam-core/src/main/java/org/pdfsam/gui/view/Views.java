/*
 * Created on 14/giu/2012
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
package org.pdfsam.gui.view;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.context.I18nContext;
import org.pdfsam.gui.support.ToolTipBuilder;
import org.pdfsam.gui.view.GradientPanel.GradientOrientation;
import org.pdfsam.gui.view.prefix.PrefixField;

import static org.apache.commons.lang3.StringUtils.isNotBlank;

/**
 * Utility class to create components
 * 
 * @author Andrea Vacondio
 * 
 */
public final class Views {

    public static final int GAP = 5;
    public static final int SMALL_GAP = 3;

    private Views() {
        // hide
    }

    /**
     * @param component
     * @param labelText
     * @param tooltip
     * @return a panel horizontally aligned with the label and the component
     */
    public static JPanel newLabeledComponent(JComponent component, String labelText, String tooltip) {
        JPanel panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
        JLabel label = new JLabel(labelText);
        if (isNotBlank(tooltip)) {
            component.setToolTipText(tooltip);
            label.setToolTipText(tooltip);
        }
        panel.add(label);
        panel.add(Box.createRigidArea(new Dimension(GAP, 0)));
        panel.add(component);
        panel.add(Box.createHorizontalGlue());
        return panel;
    }

    /**
     * 
     * @param component
     * @param labelText
     * @param tooltip
     * @return a panel horizontally aligned with the label and the component and white background
     */
    public static JPanel newLabeledComponentWhiteBackground(JComponent component, String labelText, String tooltip) {
        JPanel panel = newLabeledComponent(component, labelText, tooltip);
        panel.setBackground(Color.WHITE);
        return panel;
    }

    /**
     * @param title
     * @return a title panel with white background
     */
    public static JPanel newTitledWhitePanel(String title) {
        JPanel panel = newTitledPanel(title);
        panel.setBackground(Color.WHITE);
        return panel;
    }

    /**
     * @param title
     * @return a title panel
     */
    public static JPanel newTitledPanel(String title) {
        JPanel panel = new JPanel();
        panel.setBorder(BorderFactory.createTitledBorder(title));
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        return panel;
    }

    /**
     * 
     * @param completeForSplit
     *            if true the context menu will let the user select SplitTask related prefixs.
     * @return a new panel to let the user input a prefix
     */
    public static JPanel newPrefixPanel(boolean completeForSplit) {
        I18nContext ctx = DefaultI18nContext.getInstance();
        String title = ctx.i18n("Output prefix");
        ToolTipBuilder tp = new ToolTipBuilder();
        tp.append(ctx.i18n("Prefix for the output files name."))
                .append("It can contain special keywords like \"[TIMESTAMP]\" and they will be replaced with runtime value (e.g. the actual timestamp).")
                .append("See the context menu for the enabled keywords.");
        GradientTitledPanel titledPanel = new GradientTitledPanel(title, tp, GradientOrientation.VERTICAL);
        String labelText = ctx.i18n("Output file names prefix:");
        titledPanel.add(newLabeledComponent(new PrefixField(completeForSplit), labelText, ""), BorderLayout.CENTER);
        return titledPanel;
    }

}
