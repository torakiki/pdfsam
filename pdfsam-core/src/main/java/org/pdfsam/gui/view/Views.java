/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14/giu/2012
 * Copyright 2012 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.context.I18nContext;
import org.pdfsam.gui.support.ToolTipBuilder;
import org.pdfsam.gui.view.GradientPanel.GradientOrientation;
import org.pdfsam.gui.view.prefix.PrefixField;
import org.sejda.model.pdf.PdfVersion;

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
        tp.appendLine(ctx.i18n("Prefix for the output files name."))
                .appendLine(
                        "It can contain special keywords like \"[TIMESTAMP]\" and they will be replaced with runtime value (e.g. the actual timestamp).")
                .appendLine("See the context menu for the enabled keywords.");
        GradientTitledPanel titledPanel = new GradientTitledPanel(title, tp, GradientOrientation.VERTICAL);
        String labelText = ctx.i18n("Output file names prefix:");
        titledPanel.add(newLabeledComponent(new PrefixField(completeForSplit), labelText, ""), BorderLayout.CENTER);
        return titledPanel;
    }

    /**
     * @return a checkbox to let the user select if he wants compressed or uncompressed xref pdf documents
     */
    public static PdfVersionConstrainedCheckbox newCompressOutputCheckbox() {
        PdfVersionConstrainedCheckbox retVal = new PdfVersionConstrainedCheckbox(PdfVersion.VERSION_1_5);
        retVal.setText(DefaultI18nContext.getInstance().i18n("Compress output file/files"));
        return retVal;
    }

    /**
     * @return a checkbox to let the user decide if they want to overwrite an output file if it already exists
     */
    public static JCheckBox newOverwriteOutputCheckbox() {
        JCheckBox checkbox = new JCheckBox(DefaultI18nContext.getInstance().i18n("Overwrite if already exists"));
        checkbox.setSelected(true);
        return checkbox;
    }

    private static JPanel newButtonsPanel(List<JButton> buttons) {
        JPanel buttonPanel = new JPanel();
        buttonPanel.setLayout(new FlowLayout(FlowLayout.RIGHT, GAP, GAP));
        for (JButton current : buttons) {
            buttonPanel.add(current);
        }
        return buttonPanel;
    }

    /**
     * @param actions
     * @return a panel with right aligned buttons for the given actions
     */
    public static JPanel newButtonsPanel(AbstractAction... actions) {
        List<JButton> buttons = new ArrayList<>();
        for (AbstractAction current : actions) {
            buttons.add(new JButton(current));
        }
        return newButtonsPanel(buttons);
    }

    /**
     * @param buttons
     * @return a panel with right aligned buttons for the given buttons
     */
    public static JPanel newButtonsPanel(JButton... buttons) {
        return newButtonsPanel(Arrays.asList(buttons));
    }

}
