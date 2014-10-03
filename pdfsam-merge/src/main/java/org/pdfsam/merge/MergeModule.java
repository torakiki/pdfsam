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
package org.pdfsam.merge;

import static org.pdfsam.module.ModuleDescriptorBuilder.builder;

import java.util.function.Consumer;

import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.TitledPane;
import javafx.scene.image.ImageView;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.context.UserContext;
import org.pdfsam.module.ModuleCategory;
import org.pdfsam.module.ModuleDescriptor;
import org.pdfsam.module.ModulePriority;
import org.pdfsam.module.PdfsamModule;
import org.pdfsam.ui.io.BrowsablePdfOutputField;
import org.pdfsam.ui.io.PdfDestinationPane;
import org.pdfsam.ui.module.BaseTaskExecutionModule;
import org.pdfsam.ui.support.Views;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Merge module to let the user merge together multiple pdf documents
 * 
 * @author Andrea Vacondio
 *
 */
@PdfsamModule
public class MergeModule extends BaseTaskExecutionModule {

    private static final String MODULE_ID = "merge";

    private MergeSelectionPane selectionPane = new MergeSelectionPane(MODULE_ID);
    private MergeOptionsPane mergeOptions = new MergeOptionsPane();
    private BrowsablePdfOutputField destinationFileField;
    private PdfDestinationPane destinationPane;
    private ModuleDescriptor descriptor = builder()
            .category(ModuleCategory.MERGE)
            .name(DefaultI18nContext.getInstance().i18n("Merge"))
            .description(
                    DefaultI18nContext.getInstance().i18n(
                            "Merge together multiple pdf documents or subsections of them."))
            .priority(ModulePriority.HIGH.getPriority()).supportURL("http://www.pdfsam.org/pdf-merge").build();

    @Inject
    public MergeModule(@Named(MODULE_ID + "field") BrowsablePdfOutputField destinationFileField, @Named(MODULE_ID
            + "pane") PdfDestinationPane destinationPane) {
        this.destinationFileField = destinationFileField;
        this.destinationPane = destinationPane;
    }

    @Override
    public ModuleDescriptor descriptor() {
        return descriptor;
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

    @Override
    protected Pane getInnerPanel() {
        VBox pane = new VBox();
        pane.setAlignment(Pos.TOP_CENTER);
        VBox.setVgrow(selectionPane, Priority.ALWAYS);

        TitledPane options = Views.titledPane(DefaultI18nContext.getInstance().i18n("Merge settings"), mergeOptions);
        options.setExpanded(false);

        pane.getChildren().addAll(selectionPane, options,
                Views.titledPane(DefaultI18nContext.getInstance().i18n("Destination file"), destinationPane));
        return pane;
    }

    @Override
    public String id() {
        return MODULE_ID;
    }

    public Node graphic() {
        return new ImageView("merge.png");
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
