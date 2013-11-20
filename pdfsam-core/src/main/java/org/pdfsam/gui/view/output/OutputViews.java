/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/feb/2013
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
package org.pdfsam.gui.view.output;

import java.awt.BorderLayout;

import javax.swing.JPanel;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.context.I18nContext;
import org.pdfsam.gui.event.String;
import org.pdfsam.gui.support.ToolTipBuilder;
import org.pdfsam.gui.view.GradientPanel.GradientOrientation;
import org.pdfsam.gui.view.GradientTitledPanel;

/**
 * Utility class containing factory methods to create output views
 * 
 * @author Andrea Vacondio
 * 
 */
public final class OutputViews {

    private OutputViews() {
        // hide
    }

    /**
     * @return a titled panel for a folder destination
     */
    public static JPanel newFolderDestinationPanel() {
        I18nContext ctx = DefaultI18nContext.getInstance();
        String title = ctx.i18n("Destination folder");
        ToolTipBuilder tp = new ToolTipBuilder();
        tp.appendLine(ctx.i18n("Browse or enter the full path to the destination output folder.")).appendLine(
                "Tick the box if you want to overwrite the output file if it already exists.");
        GradientTitledPanel titledPanel = new GradientTitledPanel(title, tp, GradientOrientation.VERTICAL);
        titledPanel.add(new FolderPdfDestinationPanel(), BorderLayout.CENTER);
        return titledPanel;
    }

    public static JPanel newFilePdfDestinationPanel(String panelNamespace) {
        I18nContext ctx = DefaultI18nContext.getInstance();
        String title = ctx.i18n("Destination file");
        ToolTipBuilder tp = new ToolTipBuilder();
        tp.appendLine(ctx.i18n("Browse or enter the full path to the destination output file."))
                .appendLine("Tick the box if you want to overwrite the output file if it already exists.")
                .appendLine("Tick the box if you want compressed output files.")
                .appendLine("Set the pdf version of the ouput document.");
        GradientTitledPanel titledPanel = new GradientTitledPanel(title, tp, GradientOrientation.VERTICAL);
        FilePdfDestinationPanel panel = new FilePdfDestinationPanel();
        panel.setEventNamespace(panelNamespace);
        titledPanel.add(panel, BorderLayout.CENTER);
        return titledPanel;
    }

    /**
     * @param panel
     * @return a titled panel for the given {@link FolderPdfDestinationPanel}
     */
    public static JPanel newFolderPdfDestinationPanel(FolderPdfDestinationPanel panel) {
        I18nContext ctx = DefaultI18nContext.getInstance();
        String title = ctx.i18n("Destination folder");
        ToolTipBuilder tp = new ToolTipBuilder();
        tp.appendLine(ctx.i18n("Browse or enter the full path to the destination output directory."))
                .appendLine("Tick the box if you want to overwrite the output file if it already exists.")
                .appendLine("Tick the box if you want compressed output files.")
                .appendLine("Set the pdf version of the ouput document.");
        GradientTitledPanel titledPanel = new GradientTitledPanel(title, tp, GradientOrientation.VERTICAL);
        titledPanel.add(panel, BorderLayout.CENTER);
        return titledPanel;
    }

}
