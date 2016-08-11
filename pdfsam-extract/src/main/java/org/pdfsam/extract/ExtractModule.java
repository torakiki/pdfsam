/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 07/apr/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.extract;

import static org.pdfsam.module.ModuleDescriptorBuilder.builder;
import static org.pdfsam.ui.io.PdfDestinationPane.DestinationPanelFields.DISCARD_BOOKMARKS;
import static org.pdfsam.ui.support.Views.titledPane;

import java.util.Map;
import java.util.function.Consumer;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.context.UserContext;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.module.ModuleCategory;
import org.pdfsam.module.ModuleDescriptor;
import org.pdfsam.module.ModuleInputOutputType;
import org.pdfsam.module.ModulePriority;
import org.pdfsam.module.PdfsamModule;
import org.pdfsam.ui.io.BrowsablePdfOutputField;
import org.pdfsam.ui.io.PdfDestinationPane;
import org.pdfsam.ui.module.BaseTaskExecutionModule;
import org.pdfsam.ui.module.Footer;
import org.pdfsam.ui.module.OpenButton;
import org.pdfsam.ui.module.RunButton;
import org.pdfsam.ui.selection.single.TaskParametersBuilderSingleSelectionPane;
import org.sejda.eventstudio.annotation.EventStation;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.image.ImageView;
import javafx.scene.layout.VBox;

/**
 * Extract module to let the user extract pages from a single PDF document
 * 
 * @author Andrea Vacondio
 *
 */
@PdfsamModule
public class ExtractModule extends BaseTaskExecutionModule {

    private static final String MODULE_ID = "extract";

    private TaskParametersBuilderSingleSelectionPane selectionPane;
    private ExtractOptionsPane extractOptions = new ExtractOptionsPane();
    private BrowsablePdfOutputField destinationFileField;
    private PdfDestinationPane destinationPane;

    private ModuleDescriptor descriptor = builder().category(ModuleCategory.SPLIT)
            .inputTypes(ModuleInputOutputType.SINGLE_PDF).name(DefaultI18nContext.getInstance().i18n("Extract"))
            .description(DefaultI18nContext.getInstance().i18n("Extract pages from a PDF document."))
            .priority(ModulePriority.DEFAULT.getPriority()).supportURL("http://www.pdfsam.org/pdf-extract-pages")
            .build();

    @Inject
    public ExtractModule(@Named(MODULE_ID + "field") BrowsablePdfOutputField destinationFileField,
            @Named(MODULE_ID + "pane") PdfDestinationPane destinationPane, @Named(MODULE_ID + "footer") Footer footer) {
        super(footer);
        this.destinationFileField = destinationFileField;
        this.destinationPane = destinationPane;
        this.selectionPane = new TaskParametersBuilderSingleSelectionPane(id());
        this.selectionPane.setPromptText(DefaultI18nContext.getInstance().i18n("Select or drag and drop a PDF"));
        // this.selectionPane.addOnLoaded(d -> splitOptions.setMaxPages(d.pages().getValue()));
    }

    @Override
    public ModuleDescriptor descriptor() {
        return descriptor;
    }

    @Override
    protected ExtractParametersBuilder getBuilder(Consumer<String> onError) {
        ExtractParametersBuilder builder = new ExtractParametersBuilder();
        extractOptions.apply(builder, onError);
        selectionPane.apply(builder, onError);
        destinationFileField.apply(builder, onError);
        destinationPane.apply(builder, onError);
        return builder;
    }

    @Override
    public void onSaveWorkspace(Map<String, String> data) {
        selectionPane.saveStateTo(data);
        extractOptions.saveStateTo(data);
        destinationPane.saveStateTo(data);
        destinationFileField.saveStateTo(data);
    }

    @Override
    public void onLoadWorkspace(Map<String, String> data) {
        selectionPane.restoreStateFrom(data);
        extractOptions.restoreStateFrom(data);
        destinationPane.restoreStateFrom(data);
        destinationFileField.restoreStateFrom(data);
    }

    @Override
    protected VBox getInnerPanel() {
        VBox pane = new VBox();
        pane.setAlignment(Pos.TOP_CENTER);

        pane.getChildren().addAll(selectionPane,
                titledPane(DefaultI18nContext.getInstance().i18n("Extract settings"), extractOptions),
                titledPane(DefaultI18nContext.getInstance().i18n("Destination file"), destinationPane));
        return pane;
    }

    @Override
    @EventStation
    public String id() {
        return MODULE_ID;
    }

    @Override
    public Node graphic() {
        return new ImageView("extract.png");
    }

    @Configuration
    public static class ModuleConfig {
        @Bean(name = MODULE_ID + "field")
        public BrowsablePdfOutputField destinationFileField() {
            return new BrowsablePdfOutputField();
        }

        @Bean(name = MODULE_ID + "pane")
        public PdfDestinationPane destinationPane(@Named(MODULE_ID + "field") BrowsablePdfOutputField outputField,
                UserContext userContext) {
            PdfDestinationPane panel = new PdfDestinationPane(outputField, MODULE_ID, userContext, DISCARD_BOOKMARKS);
            panel.enableSameAsSourceItem();
            return panel;
        }

        @Bean(name = MODULE_ID + "footer")
        public Footer footer(RunButton runButton, @Named(MODULE_ID + "openButton") OpenButton openButton) {
            return new Footer(runButton, openButton, MODULE_ID);
        }

        @Bean(name = MODULE_ID + "openButton")
        public OpenButton openButton() {
            return new OpenButton(MODULE_ID, ModuleInputOutputType.SINGLE_PDF);
        }
    }

}
