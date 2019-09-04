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
package org.pdfsam.merge;

import static org.pdfsam.module.ModuleDescriptorBuilder.builder;
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
import org.pdfsam.ui.commons.ClearModuleEvent;
import org.pdfsam.ui.io.BrowsablePdfOutputField;
import org.pdfsam.ui.io.PdfDestinationPane;
import org.pdfsam.ui.module.BaseTaskExecutionModule;
import org.pdfsam.ui.module.Footer;
import org.pdfsam.ui.module.OpenButton;
import org.pdfsam.ui.module.RunButton;
import org.sejda.eventstudio.annotation.EventListener;
import org.sejda.eventstudio.annotation.EventStation;
import org.sejda.injector.Auto;
import org.sejda.injector.Components;
import org.sejda.injector.Provides;

import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.image.ImageView;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;

/**
 * Merge module to let the user merge together multiple pdf documents
 * 
 * @author Andrea Vacondio
 *
 */
@Auto
public class MergeModule extends BaseTaskExecutionModule {

    private static final String MODULE_ID = "merge";

    private MergeSelectionPane selectionPane = new MergeSelectionPane(MODULE_ID);
    private MergeOptionsPane mergeOptions = new MergeOptionsPane();
    private BrowsablePdfOutputField destinationFileField;
    private PdfDestinationPane destinationPane;
    private ModuleDescriptor descriptor = builder().category(ModuleCategory.MERGE)
            .inputTypes(ModuleInputOutputType.MULTIPLE_PDF).name(DefaultI18nContext.getInstance().i18n("Merge"))
            .description(DefaultI18nContext.getInstance()
                    .i18n("Merge together multiple PDF documents or subsections of them."))
            .priority(ModulePriority.HIGH.getPriority()).supportURL("https://pdfsam.org/pdf-merge/").build();

    @Inject
    public MergeModule(@Named(MODULE_ID + "field") BrowsablePdfOutputField destinationFileField,
            @Named(MODULE_ID + "pane") PdfDestinationPane destinationPane, @Named(MODULE_ID + "footer") Footer footer) {
        super(footer);
        this.destinationFileField = destinationFileField;
        this.destinationPane = destinationPane;
        initModuleSettingsPanel(settingPanel());
    }

    @Override
    public ModuleDescriptor descriptor() {
        return descriptor;
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

        pane.getChildren().addAll(selectionPane,
                titledPane(DefaultI18nContext.getInstance().i18n("Merge settings"), mergeOptions),
                titledPane(DefaultI18nContext.getInstance().i18n("Destination file"), destinationPane));
        return pane;
    }

    @Override
    @EventStation
    public String id() {
        return MODULE_ID;
    }

    @EventListener
    public void onClearModule(ClearModuleEvent e) {
        if (e.clearEverything) {
            mergeOptions.resetView();
            destinationPane.resetView();
        }
    }

    @Override
    public Node graphic() {
        return new ImageView("merge.png");
    }

    @Components({ MergeModule.class })
    public static class ModuleConfig {
        @Provides
        @Named(MODULE_ID + "field")
        public BrowsablePdfOutputField destinationFileField() {
            return new BrowsablePdfOutputField();
        }

        @Provides
        @Named(MODULE_ID + "pane")
        public PdfDestinationPane destinationPane(@Named(MODULE_ID + "field") BrowsablePdfOutputField outputField,
                UserContext userContext) {
            return new PdfDestinationPane(outputField, MODULE_ID, userContext);
        }

        @Provides
        @Named(MODULE_ID + "footer")
        public Footer footer(RunButton runButton, @Named(MODULE_ID + "openButton") OpenButton openButton) {
            return new Footer(runButton, openButton, MODULE_ID);
        }

        @Provides
        @Named(MODULE_ID + "openButton")
        public OpenButton openButton() {
            return new OpenButton(MODULE_ID, ModuleInputOutputType.SINGLE_PDF);
        }
    }
}
