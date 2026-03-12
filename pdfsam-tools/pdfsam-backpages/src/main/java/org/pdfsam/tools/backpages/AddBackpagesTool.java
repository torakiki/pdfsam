/*
 * This file is part of the PDF Split And Merge source code
 * Created on 23/11/22
 * Copyright 2022 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.tools.backpages;

import jakarta.inject.Named;
import javafx.scene.AccessibleRole;
import javafx.scene.Node;
import javafx.scene.layout.Pane;
import org.kordamp.ikonli.javafx.FontIcon;
import org.kordamp.ikonli.unicons.UniconsLine;
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
 * A tool that inserts a PDF document (or selected pages of it) into another document, repeating the process every "n" pages
 *
 * @author Andrea Vacondio
 */
public class AddBackpagesTool implements Tool {
    static final String TOOL_ID = "addbackpages";

    private final ToolDescriptor descriptor = builder().category(ToolCategory.MERGE)
            .inputTypes(ToolInputOutputType.SINGLE_PDF).name(i18n().tr("Insert pages multiple times")).description(
                    i18n().tr(
                            "Insert the pages of a PDF document A into another document B, repeating it after a certain number of pages, resulting in B1 A B2 A B3 A etc."))
            .priority(ToolPriority.DEFAULT.getPriority()).supportURL("https://pdfsam.org/insert-pdf-multiple-times/")
            .build();

    @Override
    public ToolDescriptor descriptor() {
        return descriptor;
    }

    @Override
    public Pane panel() {
        return app().instance(AddBackpagesToolPanel.class);
    }

    @Override
    public String id() {
        return TOOL_ID;
    }

    @Override
    public Node graphic() {
        var icon = new FontIcon(UniconsLine.ALIGN_LETTER_RIGHT);
        icon.getStyleClass().addAll(this.descriptor().category().styleClass(), "tool-icon");
        icon.setAccessibleRole(AccessibleRole.IMAGE_VIEW);
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
