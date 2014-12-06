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
package org.pdfsam.rotate;

import static org.pdfsam.module.ModuleDescriptorBuilder.builder;

import java.util.Map;
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

import org.pdfsam.context.UserContext;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.module.ModuleCategory;
import org.pdfsam.module.ModuleDescriptor;
import org.pdfsam.module.ModulePriority;
import org.pdfsam.module.PdfsamModule;
import org.pdfsam.ui.io.BrowsableOutputDirectoryField;
import org.pdfsam.ui.io.PdfDestinationPane;
import org.pdfsam.ui.module.BaseTaskExecutionModule;
import org.pdfsam.ui.prefix.PrefixPane;
import org.pdfsam.ui.support.Views;
import org.pdfsam.ui.workspace.SaveWorkspaceEvent;
import org.sejda.model.prefix.Prefix;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Rotate module to let the user rotate multiple PDF documents
 * 
 * @author Andrea Vacondio
 *
 */
@PdfsamModule
public class RotateModule extends BaseTaskExecutionModule {

    private static final String MODULE_ID = "rotate";

    private RotateSelectionPane selectionPane = new RotateSelectionPane(MODULE_ID);
    private RotateOptionsPane rotateOptions = new RotateOptionsPane();
    private BrowsableOutputDirectoryField destinationDirectoryField;
    private PdfDestinationPane destinationPane;
    private PrefixPane prefix = new PrefixPane();
    private ModuleDescriptor descriptor = builder().category(ModuleCategory.OTHER)
            .name(DefaultI18nContext.getInstance().i18n("Rotate"))
            .description(DefaultI18nContext.getInstance().i18n("Rotate the pages of multiple PDF documents."))
            .priority(ModulePriority.DEFAULT.getPriority()).supportURL("http://www.pdfsam.org/pdf-rotate").build();

    @Inject
    public RotateModule(@Named(MODULE_ID + "field") BrowsableOutputDirectoryField destinationDirectoryField,
            @Named(MODULE_ID + "pane") PdfDestinationPane destinationPane) {
        this.destinationDirectoryField = destinationDirectoryField;
        this.destinationPane = destinationPane;
    }

    @Override
    public ModuleDescriptor descriptor() {
        return descriptor;
    }

    @Override
    protected RotateParametersBuilder getBuilder(Consumer<String> onError) {
        RotateParametersBuilder builder = new RotateParametersBuilder();
        selectionPane.apply(builder, onError);
        rotateOptions.apply(builder, onError);
        destinationDirectoryField.apply(builder, onError);
        destinationPane.apply(builder, onError);
        prefix.apply(builder, onError);
        return builder;
    }

    public void onSaveWorkspace(SaveWorkspaceEvent event) {
        Map<String, String> data = event.getDataForModule(MODULE_ID);
        rotateOptions.onSaveWorkspace(data);
        destinationPane.onSaveWorkspace(event);
        data.put("destination", destinationDirectoryField.getTextField().getText());
        prefix.onSaveWorkspace(data);
    }

    @Override
    protected Pane getInnerPanel() {
        VBox pane = new VBox();
        pane.setAlignment(Pos.TOP_CENTER);
        VBox.setVgrow(selectionPane, Priority.ALWAYS);

        TitledPane prefixTitled = Views
                .titledPane(DefaultI18nContext.getInstance().i18n("File names settings"), prefix);
        prefix.addMenuItemFor(Prefix.FILENUMBER);

        TitledPane options = Views.titledPane(DefaultI18nContext.getInstance().i18n("Rotate settings"), rotateOptions);

        pane.getChildren().addAll(selectionPane, options,
                Views.titledPane(DefaultI18nContext.getInstance().i18n("Destination directory"), destinationPane),
                prefixTitled);
        return pane;
    }

    @Override
    public String id() {
        return MODULE_ID;
    }

    public Node graphic() {
        return new ImageView("rotate.png");
    }

    @Configuration
    public static class ModuleConfig {
        @Bean(name = MODULE_ID + "field")
        public BrowsableOutputDirectoryField destinationDirectoryField() {
            return new BrowsableOutputDirectoryField();
        }

        @Bean(name = MODULE_ID + "pane")
        public PdfDestinationPane destinationPane(
                @Named(MODULE_ID + "field") BrowsableOutputDirectoryField outputField, UserContext userContext) {
            return new PdfDestinationPane(outputField, MODULE_ID, userContext);
        }
    }

}
