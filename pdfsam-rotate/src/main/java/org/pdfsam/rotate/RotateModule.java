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

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.context.UserContext;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.module.ModuleCategory;
import org.pdfsam.module.ModuleDescriptor;
import org.pdfsam.module.ModuleInputOutputType;
import org.pdfsam.module.ModulePriority;
import org.pdfsam.module.PdfsamModule;
import org.pdfsam.ui.io.BrowsableOutputDirectoryField;
import org.pdfsam.ui.io.PdfDestinationPane;
import org.pdfsam.ui.module.BaseTaskExecutionModule;
import org.pdfsam.ui.module.Footer;
import org.pdfsam.ui.module.OpenButton;
import org.pdfsam.ui.module.RunButton;
import org.pdfsam.ui.prefix.PrefixPane;
import org.pdfsam.ui.support.Views;
import org.sejda.eventstudio.annotation.EventStation;
import org.sejda.model.prefix.Prefix;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.TitledPane;
import javafx.scene.image.ImageView;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;

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
            .inputTypes(ModuleInputOutputType.MULTIPLE_PDF, ModuleInputOutputType.SINGLE_PDF)
            .name(DefaultI18nContext.getInstance().i18n("Rotate"))
            .description(DefaultI18nContext.getInstance().i18n("Rotate the pages of multiple PDF documents."))
            .priority(ModulePriority.DEFAULT.getPriority()).supportURL("http://www.pdfsam.org/rotate-pdf").build();

    @Inject
    public RotateModule(@Named(MODULE_ID + "field") BrowsableOutputDirectoryField destinationDirectoryField,
            @Named(MODULE_ID + "pane") PdfDestinationPane destinationPane, @Named(MODULE_ID + "footer") Footer footer) {
        super(footer);
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

    @Override
    protected VBox getInnerPanel() {
        VBox pane = new VBox();
        pane.setAlignment(Pos.TOP_CENTER);
        VBox.setVgrow(selectionPane, Priority.ALWAYS);

        TitledPane prefixTitled = Views.titledPane(DefaultI18nContext.getInstance().i18n("File names settings"),
                prefix);
        prefix.addMenuItemFor(Prefix.FILENUMBER);

        TitledPane options = Views.titledPane(DefaultI18nContext.getInstance().i18n("Rotate settings"), rotateOptions);

        pane.getChildren().addAll(selectionPane, options,
                Views.titledPane(DefaultI18nContext.getInstance().i18n("Destination directory"), destinationPane),
                prefixTitled);
        return pane;
    }

    @Override
    @EventStation
    public String id() {
        return MODULE_ID;
    }

    @Override
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
        public PdfDestinationPane destinationPane(@Named(MODULE_ID + "field") BrowsableOutputDirectoryField outputField,
                UserContext userContext) {
            return new PdfDestinationPane(outputField, MODULE_ID, userContext);
        }

        @Bean(name = MODULE_ID + "footer")
        public Footer footer(RunButton runButton, @Named(MODULE_ID + "openButton") OpenButton openButton) {
            return new Footer(runButton, openButton, MODULE_ID);
        }

        @Bean(name = MODULE_ID + "openButton")
        public OpenButton openButton() {
            return new OpenButton(MODULE_ID, ModuleInputOutputType.MULTIPLE_PDF);
        }
    }

}
