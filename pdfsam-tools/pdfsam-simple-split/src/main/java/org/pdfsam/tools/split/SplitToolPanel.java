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
package org.pdfsam.tools.split;

import jakarta.inject.Inject;
import jakarta.inject.Named;
import javafx.geometry.Pos;
import javafx.scene.control.TitledPane;
import javafx.scene.layout.VBox;
import org.apache.commons.lang3.builder.Builder;
import org.pdfsam.core.support.params.SinglePdfSourceMultipleOutputParametersBuilder;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.eventstudio.annotation.EventStation;
import org.pdfsam.model.tool.ClearToolRequest;
import org.pdfsam.ui.components.io.BrowsableOutputDirectoryField;
import org.pdfsam.ui.components.io.PdfDestinationPane;
import org.pdfsam.ui.components.prefix.PrefixPane;
import org.pdfsam.ui.components.selection.single.TaskParametersBuilderSingleSelectionPane;
import org.pdfsam.ui.components.support.Views;
import org.pdfsam.ui.components.tool.BaseToolPanel;
import org.pdfsam.ui.components.tool.Footer;
import org.sejda.model.parameter.AbstractSplitByPageParameters;
import org.sejda.model.prefix.Prefix;

import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import static org.pdfsam.i18n.I18nContext.i18n;
import static org.pdfsam.tools.split.SplitTool.TOOL_ID;

/**
 * @author Andrea Vacondio
 */
public class SplitToolPanel extends BaseToolPanel {

    private final TaskParametersBuilderSingleSelectionPane selectionPane;
    private final BrowsableOutputDirectoryField destinationDirectoryField;
    private final PdfDestinationPane destinationPane;
    private final SplitOptionsPane splitOptions = new SplitOptionsPane();
    private final PrefixPane prefix;

    @Inject
    public SplitToolPanel(@Named(TOOL_ID + "field") BrowsableOutputDirectoryField destinationDirectoryField,
            @Named(TOOL_ID + "pane") PdfDestinationPane destinationPane, @Named(TOOL_ID + "footer") Footer footer,
            @Named(TOOL_ID + "prefix") PrefixPane prefix) {
        super(TOOL_ID, footer);
        this.destinationDirectoryField = destinationDirectoryField;
        this.destinationPane = destinationPane;
        this.selectionPane = new TaskParametersBuilderSingleSelectionPane(id());
        this.selectionPane.setPromptText(i18n().tr("Select or drag and drop the PDF you want to split"));
        this.selectionPane.addOnLoaded(d -> splitOptions.setMaxPages(d.pages().getValue()));
        this.prefix = prefix;
        initModuleSettingsPanel(settingPanel());
    }

    @Override
    public void onSaveWorkspace(Map<String, String> data) {
        selectionPane.saveStateTo(data);
        splitOptions.saveStateTo(data);
        destinationDirectoryField.saveStateTo(data);
        destinationPane.saveStateTo(data);
        prefix.saveStateTo(data);
    }

    @Override
    public void onLoadWorkspace(Map<String, String> data) {
        selectionPane.restoreStateFrom(data);
        splitOptions.restoreStateFrom(data);
        destinationDirectoryField.restoreStateFrom(data);
        destinationPane.restoreStateFrom(data);
        prefix.restoreStateFrom(data);
    }

    @Override
    protected Builder<? extends AbstractSplitByPageParameters> getBuilder(Consumer<String> onError) {
        Optional<SinglePdfSourceMultipleOutputParametersBuilder<? extends AbstractSplitByPageParameters>> builder = Optional.ofNullable(
                splitOptions.getBuilder(onError));
        builder.ifPresent(b -> {
            selectionPane.apply(b, onError);
            destinationDirectoryField.apply(b, onError);
            destinationPane.apply(b, onError);
            prefix.apply(b, onError);
        });
        return builder.orElse(null);
    }

    private VBox settingPanel() {
        VBox pane = new VBox();
        pane.setAlignment(Pos.TOP_CENTER);

        TitledPane prefixTitled = Views.titledPane(i18n().tr("File names settings"), prefix);
        prefix.addMenuItemFor(Prefix.CURRENTPAGE);
        prefix.addMenuItemFor(Prefix.FILENUMBER);
        prefix.addMenuItemFor("[TOTAL_FILESNUMBER]");

        pane.getChildren().addAll(selectionPane, Views.titledPane(i18n().tr("Split settings"), splitOptions),
                Views.titledPane(i18n().tr("Output settings"), destinationPane), prefixTitled);
        return pane;
    }

    @EventListener
    public void onClearModule(ClearToolRequest e) {
        if (e.clearEverything()) {
            splitOptions.resetView();
            prefix.resetView();
            destinationPane.resetView();
        }
    }

    @EventStation
    public String id() {
        return TOOL_ID;
    }
}
