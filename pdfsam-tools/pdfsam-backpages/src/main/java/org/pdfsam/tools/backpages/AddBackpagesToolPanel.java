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

import jakarta.inject.Inject;
import jakarta.inject.Named;
import javafx.geometry.Pos;
import javafx.scene.layout.VBox;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.eventstudio.annotation.EventStation;
import org.pdfsam.model.tool.ClearToolRequest;
import org.pdfsam.ui.components.io.BrowsablePdfOutputField;
import org.pdfsam.ui.components.io.PdfDestinationPane;
import org.pdfsam.ui.components.selection.single.TaskParametersBuilderSingleSelectionPane;
import org.pdfsam.ui.components.support.FXValidationSupport;
import org.pdfsam.ui.components.support.Views;
import org.pdfsam.ui.components.tool.BaseToolPanel;
import org.pdfsam.ui.components.tool.Footer;
import org.sejda.model.output.FileOrDirectoryTaskOutput;

import java.io.File;
import java.util.Map;
import java.util.function.Consumer;

import static org.pdfsam.i18n.I18nContext.i18n;
import static org.pdfsam.tools.backpages.AddBackpagesTool.TOOL_ID;

/**
 * @author Andrea Vacondio
 */
public class AddBackpagesToolPanel extends BaseToolPanel {

    private final TaskParametersBuilderSingleSelectionPane selectionPane;
    private final BrowsablePdfOutputField destinationFileField;
    private final PdfDestinationPane destinationPane;
    private final AddBackpagesPane addBackpagesOptions;

    @Inject
    public AddBackpagesToolPanel(@Named(TOOL_ID + "field") BrowsablePdfOutputField destinationFileField,
            @Named(TOOL_ID + "pane") PdfDestinationPane destinationPane, @Named(TOOL_ID + "footer") Footer footer) {
        super(TOOL_ID, footer);
        this.destinationFileField = destinationFileField;
        this.destinationPane = destinationPane;
        this.addBackpagesOptions = new AddBackpagesPane(TOOL_ID);
        this.selectionPane = new TaskParametersBuilderSingleSelectionPane(id());
        this.selectionPane.setPromptText(i18n().tr("Select or drag and drop the PDF you want to insert pages to"));
        initModuleSettingsPanel(settingPanel());
    }

    @Override
    public void onSaveWorkspace(Map<String, String> data) {
        selectionPane.saveStateTo(data);
        addBackpagesOptions.saveStateTo(data);
        destinationFileField.saveStateTo(data);
        destinationPane.saveStateTo(data);
    }

    @Override
    public void onLoadWorkspace(Map<String, String> data) {
        selectionPane.restoreStateFrom(data);
        addBackpagesOptions.restoreStateFrom(data);
        destinationFileField.restoreStateFrom(data);
        destinationPane.restoreStateFrom(data);
    }

    @Override
    protected AddBackpagesParametersBuilder getBuilder(Consumer<String> onError) {
        var builder = new AddBackpagesParametersBuilder();
        selectionPane.apply(builder, onError);
        addBackpagesOptions.apply(builder, onError);
        var destinationField = destinationFileField.getTextField();
        destinationField.validate();
        if (destinationField.getValidationState() == FXValidationSupport.ValidationState.VALID) {
            builder.output(FileOrDirectoryTaskOutput.file(new File(destinationField.getText())));
        } else {
            onError.accept(i18n().tr("A .pdf destination file extension is required"));
        }
        destinationPane.apply(builder, onError);
        return builder;
    }

    private VBox settingPanel() {
        VBox pane = new VBox();
        pane.setAlignment(Pos.TOP_CENTER);
        pane.getChildren()
                .addAll(selectionPane, Views.titledPane(i18n().tr("Insert and Repeat settings"), addBackpagesOptions),
                        Views.titledPane(i18n().tr("Destination file"), destinationPane));
        return pane;
    }

    @EventStation
    public String id() {
        return TOOL_ID;
    }

    @EventListener
    public void onClearModule(ClearToolRequest e) {
        if (e.clearEverything()) {
            addBackpagesOptions.resetView();
            destinationPane.resetView();
        }
    }
}
