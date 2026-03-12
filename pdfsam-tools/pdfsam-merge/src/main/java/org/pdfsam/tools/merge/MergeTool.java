/*
 * This file is part of the PDF Split And Merge source code
 * Created on 07/apr/2014
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.tools.merge;

import jakarta.inject.Named;
import javafx.scene.AccessibleRole;
import javafx.scene.Node;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.Pane;
import org.kordamp.ikonli.javafx.FontIcon;
import org.kordamp.ikonli.unicons.UniconsLine;
import org.pdfsam.injector.Auto;
import org.pdfsam.injector.Provides;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.model.tool.ToolCategory;
import org.pdfsam.model.tool.ToolDescriptor;
import org.pdfsam.model.tool.ToolInputOutputType;
import org.pdfsam.model.tool.ToolPriority;
import org.pdfsam.ui.components.io.BrowsablePdfOutputField;
import org.pdfsam.ui.components.io.PdfDestinationPane;
import org.pdfsam.ui.components.tool.Footer;
import org.pdfsam.ui.components.tool.OpenButton;
import org.pdfsam.ui.components.tool.RunButton;

import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.i18n.I18nContext.i18n;
import static org.pdfsam.model.tool.ToolDescriptorBuilder.builder;

/**
 * Merge module to let the user merge together multiple pdf documents
 *
 * @author Andrea Vacondio
 */
@Auto
public class MergeTool implements Tool {

    static final String TOOL_ID = "merge";

    private final ToolDescriptor descriptor = builder().category(ToolCategory.MERGE)
            .inputTypes(ToolInputOutputType.MULTIPLE_PDF).name(i18n().tr("Merge"))
            .description(i18n().tr("Merge multiple PDF documents or subsections of them."))
            .priority(ToolPriority.HIGH.getPriority()).supportURL("https://pdfsam.org/pdf-merge/")
            .build();

    @Override
    public ToolDescriptor descriptor() {
        return descriptor;
    }

    @Override
    public Pane panel() {
        return app().instance(MergeToolPanel.class);
    }

    @Override
    public String id() {
        return TOOL_ID;
    }

    @Override
    public Node graphic() {
        var icon = new FontIcon(UniconsLine.FILES_LANDSCAPES_ALT);
        icon.getStyleClass().addAll(this.descriptor().category().styleClass(), "tool-icon");
        icon.setAccessibleText(i18n().tr("Merge"));
        icon.setAccessibleRole(AccessibleRole.IMAGE_VIEW);
        Tooltip.install(icon, new Tooltip(i18n().tr("Merge")));
        return icon;
    }

    public static class ModuleConfig {
        @Provides
        @Named(TOOL_ID + "field")
        public BrowsablePdfOutputField destinationFileField() {
            return new BrowsablePdfOutputField();
        }

        @Provides
        @Named(TOOL_ID + "pane")
        public PdfDestinationPane destinationPane(@Named(TOOL_ID + "field") BrowsablePdfOutputField outputField) {
            return new PdfDestinationPane(outputField, TOOL_ID);
        }

        @Provides
        @Named(TOOL_ID + "footer")
        public Footer footer(RunButton runButton, @Named(TOOL_ID + "openButton") OpenButton openButton) {
            return new Footer(runButton, openButton, TOOL_ID);
        }

        @Provides
        @Named(TOOL_ID + "openButton")
        public OpenButton openButton() {
            return new OpenButton(TOOL_ID, ToolInputOutputType.SINGLE_PDF);
        }
    }
}
