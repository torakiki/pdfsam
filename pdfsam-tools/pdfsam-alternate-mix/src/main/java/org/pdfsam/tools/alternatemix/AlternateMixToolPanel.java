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
package org.pdfsam.tools.alternatemix;

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
import org.pdfsam.ui.components.support.Views;
import org.pdfsam.ui.components.tool.BaseToolPanel;
import org.pdfsam.ui.components.tool.Footer;

import java.util.Map;
import java.util.function.Consumer;

import static org.pdfsam.i18n.I18nContext.i18n;
import static org.pdfsam.tools.alternatemix.AlternateMixTool.TOOL_ID;

/**
 * @author Andrea Vacondio
 */
public class AlternateMixToolPanel extends BaseToolPanel {

    private final AlternateMixSelectionPane selectionPane = new AlternateMixSelectionPane(TOOL_ID);
    private final BrowsablePdfOutputField destinationFileField;
    private final PdfDestinationPane destinationPane;

    @Inject
    public AlternateMixToolPanel(@Named(TOOL_ID + "field") BrowsablePdfOutputField destinationFileField,
            @Named(TOOL_ID + "pane") PdfDestinationPane destinationPane, @Named(TOOL_ID + "footer") Footer footer) {
        super(TOOL_ID, footer);
        this.destinationFileField = destinationFileField;
        this.destinationPane = destinationPane;
        initModuleSettingsPanel(settingPanel());
    }

    @Override
    public void onSaveWorkspace(Map<String, String> data) {
        selectionPane.saveStateTo(data);
        destinationFileField.saveStateTo(data);
        destinationPane.saveStateTo(data);
    }

    @Override
    public void onLoadWorkspace(Map<String, String> data) {
        // backwards comp when alternate mix had 2 inputs
        if (data.containsKey("firstDocumentMixinput")) {
            data.put("input.0", data.get("firstDocumentMixinput"));
            data.put("input.password.0", data.get("firstDocumentMixinputinput.password"));
            data.put("input.step.0", data.get("firstStep"));
            data.put("input.reverse.0", data.get("reverseFirst"));
            data.put("input.size", "1");
            if (data.containsKey("secondDocumentMixinput")) {
                data.put("input.1", data.get("secondDocumentMixinput"));
                data.put("input.password.1", data.get("secondDocumentMixinput.password"));
                data.put("input.step.1", data.get("secondStep"));
                data.put("input.reverse.1", data.get("reverseSecond"));
                data.put("input.size", "2");
            }
        }
        selectionPane.restoreStateFrom(data);
        destinationFileField.restoreStateFrom(data);
        destinationPane.restoreStateFrom(data);
    }

    @Override
    protected AlternateMixParametersBuilder getBuilder(Consumer<String> onError) {
        AlternateMixParametersBuilder builder = new AlternateMixParametersBuilder();
        selectionPane.apply(builder, onError);
        destinationFileField.apply(builder, onError);
        destinationPane.apply(builder, onError);
        return builder;
    }

    private VBox settingPanel() {
        VBox pane = new VBox();
        pane.setAlignment(Pos.TOP_CENTER);
        VBox.setVgrow(selectionPane, Priority.ALWAYS);
        pane.getChildren().addAll(selectionPane, Views.titledPane(i18n().tr("Destination file"), destinationPane));
        return pane;
    }

    @EventStation
    public String id() {
        return TOOL_ID;
    }

    @EventListener
    public void onClearModule(ClearToolRequest e) {
        if (e.clearEverything()) {
            destinationPane.resetView();
        }
    }
}
