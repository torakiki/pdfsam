/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 07/apr/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.split;

import jakarta.inject.Inject;
import jakarta.inject.Named;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.TitledPane;
import javafx.scene.image.ImageView;
import javafx.scene.layout.VBox;
import org.apache.commons.lang3.builder.Builder;
import org.pdfsam.context.UserContext;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.eventstudio.annotation.EventStation;
import org.pdfsam.i18n.I18nContext;
import org.pdfsam.injector.Auto;
import org.pdfsam.injector.Components;
import org.pdfsam.injector.Provides;
import org.pdfsam.module.ToolCategory;
import org.pdfsam.module.ToolDescriptor;
import org.pdfsam.module.ToolInputOutputType;
import org.pdfsam.module.ToolPriority;
import org.pdfsam.support.params.SinglePdfSourceMultipleOutputParametersBuilder;
import org.pdfsam.ui.commons.ClearModuleEvent;
import org.pdfsam.ui.io.BrowsableOutputDirectoryField;
import org.pdfsam.ui.io.PdfDestinationPane;
import org.pdfsam.ui.module.BaseTaskExecutionTool;
import org.pdfsam.ui.module.Footer;
import org.pdfsam.ui.module.OpenButton;
import org.pdfsam.ui.module.RunButton;
import org.pdfsam.ui.prefix.PrefixPane;
import org.pdfsam.ui.selection.single.TaskParametersBuilderSingleSelectionPane;
import org.pdfsam.ui.support.Views;
import org.sejda.model.parameter.AbstractSplitByPageParameters;
import org.sejda.model.prefix.Prefix;

import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import static org.pdfsam.module.ToolDescriptorBuilder.builder;
import static org.pdfsam.ui.io.PdfDestinationPane.DestinationPanelFields.DISCARD_BOOKMARKS;

/**
 * Simple split module to let the user set page numbers to split an input pdf document.
 *
 * @author Andrea Vacondio
 */
@Auto
public class SplitTool extends BaseTaskExecutionTool {

    private static final String MODULE_ID = "split.simple";

    private TaskParametersBuilderSingleSelectionPane selectionPane;
    private BrowsableOutputDirectoryField destinationDirectoryField;
    private PdfDestinationPane destinationPane;
    private SplitOptionsPane splitOptions = new SplitOptionsPane();
    private PrefixPane prefix;
    private ToolDescriptor descriptor = builder().category(ToolCategory.SPLIT)
            .inputTypes(ToolInputOutputType.SINGLE_PDF).name(i18n().tr("Split"))
            .description(i18n().tr("Split a PDF document at the given page numbers."))
            .priority(ToolPriority.HIGH.getPriority()).supportURL("https://pdfsam.org/pdf-split/")
            .build();

    @Inject
    public SplitTool(@Named(MODULE_ID + "field") BrowsableOutputDirectoryField destinationDirectoryField,
            @Named(MODULE_ID + "pane") PdfDestinationPane destinationPane, @Named(MODULE_ID + "footer") Footer footer,
            @Named(MODULE_ID + "prefix") PrefixPane prefix) {
        super(footer);
        this.destinationDirectoryField = destinationDirectoryField;
        this.destinationPane = destinationPane;
        this.selectionPane = new TaskParametersBuilderSingleSelectionPane(id());
        this.selectionPane.setPromptText(i18n().tr("Select or drag and drop the PDF you want to split"));
        this.selectionPane.addOnLoaded(d -> splitOptions.setMaxPages(d.pages().getValue()));
        this.prefix = prefix;
        initModuleSettingsPanel(settingPanel());
    }

    @Override
    public ToolDescriptor descriptor() {
        return descriptor;
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
        Optional<SinglePdfSourceMultipleOutputParametersBuilder<? extends AbstractSplitByPageParameters>> builder = Optional
                .ofNullable(splitOptions.getBuilder(onError));
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
    public void onClearModule(ClearModuleEvent e) {
        if (e.clearEverything) {
            splitOptions.resetView();
            prefix.resetView();
            destinationPane.resetView();
        }
    }

    @Override
    @EventStation
    public String id() {
        return MODULE_ID;
    }

    @Override
    public Node graphic() {
        return new ImageView("split.png");
    }

    @Components({ SplitTool.class })
    public static class ModuleConfig {
        @Provides
        @Named(MODULE_ID + "field")
        public BrowsableOutputDirectoryField destinationDirectoryField() {
            return new BrowsableOutputDirectoryField();
        }

        @Provides
        @Named(MODULE_ID + "pane")
        public PdfDestinationPane destinationPane(@Named(MODULE_ID + "field") BrowsableOutputDirectoryField outputField,
                UserContext userContext) {
            PdfDestinationPane panel = new PdfDestinationPane(outputField, MODULE_ID, userContext, DISCARD_BOOKMARKS);
            panel.enableSameAsSourceItem();
            return panel;
        }

        @Provides
        @Named(MODULE_ID + "footer")
        public Footer footer(RunButton runButton, @Named(MODULE_ID + "openButton") OpenButton openButton) {
            return new Footer(runButton, openButton, MODULE_ID);
        }

        @Provides
        @Named(MODULE_ID + "openButton")
        public OpenButton openButton() {
            return new OpenButton(MODULE_ID, ToolInputOutputType.MULTIPLE_PDF);
        }

        @Provides
        @Named(MODULE_ID + "prefix")
        public PrefixPane prefixPane(UserContext userContext) {
            return new PrefixPane(MODULE_ID, userContext);
        }
    }
}
