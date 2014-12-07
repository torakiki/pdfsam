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
package org.pdfsam.alternatemix;

import static org.pdfsam.module.ModuleDescriptorBuilder.builder;

import java.util.Map;
import java.util.function.Consumer;

import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.image.ImageView;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.context.UserContext;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.module.ModuleCategory;
import org.pdfsam.module.ModuleDescriptor;
import org.pdfsam.module.ModulePriority;
import org.pdfsam.module.PdfsamModule;
import org.pdfsam.ui.io.BrowsablePdfOutputField;
import org.pdfsam.ui.io.PdfDestinationPane;
import org.pdfsam.ui.module.BaseTaskExecutionModule;
import org.pdfsam.ui.support.Views;
import org.pdfsam.ui.workspace.SaveWorkspaceEvent;
import org.sejda.eventstudio.annotation.EventStation;
import org.sejda.model.input.PdfFileSource;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Alternate mix module to let the user merge two pdf documents taking pages alternately in straight or reverse order.
 * 
 * @author Andrea Vacondio
 *
 */
@PdfsamModule
public class AlternateMixModule extends BaseTaskExecutionModule {

    private static final String MODULE_ID = "alternatemix";

    private AlternateMixSingleSelectionPane firstDocument;
    private AlternateMixSingleSelectionPane secondDocument;
    private BrowsablePdfOutputField destinationFileField;
    private PdfDestinationPane destinationPane;
    private AlternateMixOptionsPane optionsPane = new AlternateMixOptionsPane();
    private ModuleDescriptor descriptor = builder()
            .category(ModuleCategory.MERGE)
            .name(DefaultI18nContext.getInstance().i18n("Alternate Mix"))
            .description(
                    DefaultI18nContext.getInstance().i18n(
                            "Merge two PDF documents taking pages alternately in straight or reverse order."))
            .priority(ModulePriority.DEFAULT.getPriority()).supportURL("http://www.pdfsam.org/alternate-mix").build();

    @Inject
    public AlternateMixModule(@Named(MODULE_ID + "field") BrowsablePdfOutputField destinationFileField,
            @Named(MODULE_ID + "pane") PdfDestinationPane destinationPane) {
        this.destinationFileField = destinationFileField;
        this.destinationPane = destinationPane;
        this.firstDocument = new AlternateMixSingleSelectionPane(id()) {
            @Override
            void onValidSource(AlternateMixParametersBuilder builder, PdfFileSource source) {
                builder.first(source);
            }
        };
        this.firstDocument.setId("firstDocumentMix");
        this.firstDocument.setPromptText(DefaultI18nContext.getInstance().i18n(
                "Select or drag and drop the first PDF you want to mix"));
        this.secondDocument = new AlternateMixSingleSelectionPane(id()) {
            @Override
            void onValidSource(AlternateMixParametersBuilder builder, PdfFileSource source) {
                builder.second(source);
            }
        };
        this.secondDocument.setId("secondDocumentMix");
        this.secondDocument.setPromptText(DefaultI18nContext.getInstance().i18n(
                "Select or drag and drop the second PDF you want to mix"));

    }

    @Override
    public ModuleDescriptor descriptor() {
        return descriptor;
    }

    public void onSaveWorkspace(SaveWorkspaceEvent event) {
        Map<String, String> data = event.getDataForModule(MODULE_ID);
        firstDocument.saveStateTo(data);
        secondDocument.saveStateTo(data);
        optionsPane.saveStateTo(data);
        destinationFileField.saveStateTo(data);
        destinationPane.saveStateTo(data);
    }

    @Override
    protected Pane getInnerPanel() {
        VBox pane = new VBox();
        pane.setAlignment(Pos.TOP_CENTER);

        pane.getChildren().addAll(firstDocument, secondDocument,
                Views.titledPane(DefaultI18nContext.getInstance().i18n("Mix settings"), optionsPane),
                Views.titledPane(DefaultI18nContext.getInstance().i18n("Destination file"), destinationPane));
        return pane;
    }

    @Override
    @EventStation
    public String id() {
        return MODULE_ID;
    }

    public Node graphic() {
        return new ImageView("alternate_mix.png");
    }

    @Override
    protected AlternateMixParametersBuilder getBuilder(Consumer<String> onError) {
        AlternateMixParametersBuilder builder = new AlternateMixParametersBuilder();
        firstDocument.apply(builder, onError);
        secondDocument.apply(builder, onError);
        destinationFileField.apply(builder, onError);
        destinationPane.apply(builder, onError);
        optionsPane.apply(builder, onError);
        return builder;
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
            return new PdfDestinationPane(outputField, MODULE_ID, userContext);
        }
    }

}
