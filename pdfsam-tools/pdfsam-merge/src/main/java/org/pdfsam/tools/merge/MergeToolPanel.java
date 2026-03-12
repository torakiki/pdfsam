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
package org.pdfsam.tools.merge;

import jakarta.inject.Inject;
import jakarta.inject.Named;
import javafx.geometry.Pos;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.eventstudio.annotation.EventStation;
import org.pdfsam.model.tool.ClearToolRequest;
import org.pdfsam.ui.components.io.BrowsablePdfOutputField;
import org.pdfsam.ui.components.io.PdfDestinationPane;
import org.pdfsam.ui.components.tool.BaseToolPanel;
import org.pdfsam.ui.components.tool.Footer;

import java.util.Map;
import java.util.function.Consumer;

import static org.pdfsam.i18n.I18nContext.i18n;
import static org.pdfsam.tools.merge.MergeTool.TOOL_ID;
import static org.pdfsam.ui.components.support.Views.titledPane;

/**
 * @author Andrea Vacondio
 */
public class MergeToolPanel extends BaseToolPanel {
    private final MergeSelectionPane selectionPane = new MergeSelectionPane(TOOL_ID);
    private final MergeOptionsPane mergeOptions = new MergeOptionsPane();
    private final BrowsablePdfOutputField destinationFileField;
    private final PdfDestinationPane destinationPane;

    @Inject
    public MergeToolPanel(@Named(TOOL_ID + "field") BrowsablePdfOutputField destinationFileField,
            @Named(TOOL_ID + "pane") PdfDestinationPane destinationPane, @Named(TOOL_ID + "footer") Footer footer) {
        super(TOOL_ID, footer);
        this.destinationFileField = destinationFileField;
        this.destinationPane = destinationPane;
        initModuleSettingsPanel(settingPanel());
    }

    @Override
    public void onSaveWorkspace(Map<String, String> data) {
        selectionPane.saveStateTo(data);
        mergeOptions.saveStateTo(data);
        destinationFileField.saveStateTo(data);
        destinationPane.saveStateTo(data);
    }

    @Override
    public void onLoadWorkspace(Map<String, String> data) {
        selectionPane.restoreStateFrom(data);
        mergeOptions.restoreStateFrom(data);
        destinationFileField.restoreStateFrom(data);
        destinationPane.restoreStateFrom(data);
    }

    @Override
    protected MergeParametersBuilder getBuilder(Consumer<String> onError) {
        MergeParametersBuilder builder = new MergeParametersBuilder();
        selectionPane.apply(builder, onError);
        mergeOptions.apply(builder, onError);
        destinationFileField.apply(builder, onError);
        destinationPane.apply(builder, onError);
        return builder;
    }

    private VBox settingPanel() {
        VBox pane = new VBox();
        pane.setAlignment(Pos.TOP_CENTER);
        VBox.setVgrow(selectionPane, Priority.ALWAYS);

        pane.getChildren().addAll(selectionPane, titledPane(i18n().tr("Merge settings"), mergeOptions),
                titledPane(i18n().tr("Destination file"), destinationPane));
        return pane;
    }

    @EventStation
    public String id() {
        return TOOL_ID;
    }

    @EventListener
    public void onClearModule(ClearToolRequest e) {
        if (e.clearEverything()) {
            mergeOptions.resetView();
            destinationPane.resetView();
        }
    }

}
