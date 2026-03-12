/*
 * This file is part of the PDF Split And Merge source code
 * Created on 03/10/22
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
package org.pdfsam.tools.rotate;

import jakarta.inject.Inject;
import jakarta.inject.Named;
import javafx.geometry.Pos;
import javafx.scene.control.TitledPane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.eventstudio.annotation.EventStation;
import org.pdfsam.model.tool.ClearToolRequest;
import org.pdfsam.ui.components.io.BrowsableOutputDirectoryField;
import org.pdfsam.ui.components.io.PdfDestinationPane;
import org.pdfsam.ui.components.prefix.PrefixPane;
import org.pdfsam.ui.components.support.Views;
import org.pdfsam.ui.components.tool.BaseToolPanel;
import org.pdfsam.ui.components.tool.Footer;
import org.sejda.model.prefix.Prefix;

import java.util.Map;
import java.util.function.Consumer;

import static org.pdfsam.i18n.I18nContext.i18n;
import static org.pdfsam.tools.rotate.RotateTool.TOOL_ID;

/**
 * @author Andrea Vacondio
 */
public class RotateToolPanel extends BaseToolPanel {
    private final RotateSelectionPane selectionPane = new RotateSelectionPane(TOOL_ID);
    private final RotateOptionsPane rotateOptions = new RotateOptionsPane();
    private final BrowsableOutputDirectoryField destinationDirectoryField;
    private final PdfDestinationPane destinationPane;
    private final PrefixPane prefix;

    @Inject
    public RotateToolPanel(@Named(TOOL_ID + "field") BrowsableOutputDirectoryField destinationDirectoryField,
            @Named(TOOL_ID + "pane") PdfDestinationPane destinationPane, @Named(TOOL_ID + "footer") Footer footer,
            @Named(TOOL_ID + "prefix") PrefixPane prefix) {
        super(TOOL_ID, footer);
        this.destinationDirectoryField = destinationDirectoryField;
        this.destinationPane = destinationPane;
        this.prefix = prefix;
        initModuleSettingsPanel(settingPanel());
    }

    @Override
    protected RotateParametersBuilder getBuilder(Consumer<String> onError) {
        RotateParametersBuilder builder = new RotateParametersBuilder();
        rotateOptions.apply(builder, onError);
        selectionPane.apply(builder, onError);
        destinationDirectoryField.apply(builder, onError);
        destinationPane.apply(builder, onError);
        prefix.apply(builder, onError);
        return builder;
    }

    @Override
    public void onSaveWorkspace(Map<String, String> data) {
        selectionPane.saveStateTo(data);
        rotateOptions.saveStateTo(data);
        destinationPane.saveStateTo(data);
        destinationDirectoryField.saveStateTo(data);
        prefix.saveStateTo(data);
    }

    @Override
    public void onLoadWorkspace(Map<String, String> data) {
        selectionPane.restoreStateFrom(data);
        rotateOptions.restoreStateFrom(data);
        destinationPane.restoreStateFrom(data);
        destinationDirectoryField.restoreStateFrom(data);
        prefix.restoreStateFrom(data);
    }

    private VBox settingPanel() {
        VBox pane = new VBox();
        pane.setAlignment(Pos.TOP_CENTER);
        VBox.setVgrow(selectionPane, Priority.ALWAYS);

        TitledPane prefixTitled = Views.titledPane(i18n().tr("File names settings"), prefix);
        prefix.addMenuItemFor(Prefix.FILENUMBER);
        prefix.addMenuItemFor("[TOTAL_FILESNUMBER]");

        TitledPane options = Views.titledPane(i18n().tr("Rotate settings"), rotateOptions);

        pane.getChildren()
                .addAll(selectionPane, options, Views.titledPane(i18n().tr("Output settings"), destinationPane),
                        prefixTitled);
        return pane;
    }

    @EventStation
    public String id() {
        return TOOL_ID;
    }

    @EventListener
    public void onClearModule(ClearToolRequest e) {
        if (e.clearEverything()) {
            rotateOptions.resetView();
            prefix.resetView();
            destinationPane.resetView();
        }
    }
}
