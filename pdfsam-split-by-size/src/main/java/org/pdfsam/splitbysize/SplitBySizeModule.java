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
package org.pdfsam.splitbysize;

import static org.pdfsam.module.ModuleDescriptorBuilder.builder;
import static org.pdfsam.ui.io.PdfDestinationPane.DestinationPanelFields.DISCARD_BOOKMARKS;

import java.util.Map;
import java.util.function.Consumer;

import javax.inject.Inject;
import javax.inject.Named;

import org.apache.commons.lang3.builder.Builder;
import org.pdfsam.context.UserContext;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.module.ModuleCategory;
import org.pdfsam.module.ModuleDescriptor;
import org.pdfsam.module.ModuleInputOutputType;
import org.pdfsam.module.ModulePriority;
import org.pdfsam.ui.io.BrowsableOutputDirectoryField;
import org.pdfsam.ui.io.PdfDestinationPane;
import org.pdfsam.ui.module.BaseTaskExecutionModule;
import org.pdfsam.ui.module.Footer;
import org.pdfsam.ui.module.OpenButton;
import org.pdfsam.ui.module.RunButton;
import org.pdfsam.ui.prefix.PrefixPane;
import org.pdfsam.ui.selection.single.TaskParametersBuilderSingleSelectionPane;
import org.pdfsam.ui.support.Views;
import org.sejda.eventstudio.annotation.EventStation;
import org.sejda.injector.Auto;
import org.sejda.injector.Components;
import org.sejda.injector.Provides;
import org.sejda.model.parameter.SplitBySizeParameters;
import org.sejda.model.prefix.Prefix;

import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.TitledPane;
import javafx.scene.image.ImageView;
import javafx.scene.layout.VBox;

/**
 * Split by size module to let the user split a pdf documents into documents of the given size.
 * 
 * @author Andrea Vacondio
 *
 */
@Auto
public class SplitBySizeModule extends BaseTaskExecutionModule {

    private static final String MODULE_ID = "split.bysize";

    private TaskParametersBuilderSingleSelectionPane selectionPane;
    private BrowsableOutputDirectoryField destinationDirectoryField;
    private PdfDestinationPane destinationPane;
    private SplitOptionsPane splitOptions = new SplitOptionsPane();
    private PrefixPane prefix = new PrefixPane();
    private ModuleDescriptor descriptor = builder().category(ModuleCategory.SPLIT)
            .inputTypes(ModuleInputOutputType.SINGLE_PDF).name(DefaultI18nContext.getInstance().i18n("Split by size"))
            .description(
                    DefaultI18nContext.getInstance().i18n("Split a PDF document in files of the given size (roughly)."))
            .priority(ModulePriority.LOW.getPriority()).supportURL("http://www.pdfsam.org/pdf-split-by-size").build();

    @Inject
    public SplitBySizeModule(@Named(MODULE_ID + "field") BrowsableOutputDirectoryField destinationDirectoryField,
            @Named(MODULE_ID + "pane") PdfDestinationPane destinationPane, @Named(MODULE_ID + "footer") Footer footer) {
        super(footer);
        this.destinationDirectoryField = destinationDirectoryField;
        this.destinationPane = destinationPane;
        this.selectionPane = new TaskParametersBuilderSingleSelectionPane(id());
        this.selectionPane.setPromptText(
                DefaultI18nContext.getInstance().i18n("Select or drag and drop the PDF you want to split"));
        initModuleSettingsPanel(settingPanel());
    }

    @Override
    public ModuleDescriptor descriptor() {
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
    protected Builder<SplitBySizeParameters> getBuilder(Consumer<String> onError) {
        SplitBySizeParametersBuilder builder = new SplitBySizeParametersBuilder();
        splitOptions.apply(builder, onError);
        selectionPane.apply(builder, onError);
        destinationDirectoryField.apply(builder, onError);
        destinationPane.apply(builder, onError);
        prefix.apply(builder, onError);
        return builder;
    }

    private VBox settingPanel() {
        VBox pane = new VBox();
        pane.setAlignment(Pos.TOP_CENTER);

        TitledPane prefixTitled = Views.titledPane(DefaultI18nContext.getInstance().i18n("File names settings"),
                prefix);
        prefix.addMenuItemFor(Prefix.CURRENTPAGE);
        prefix.addMenuItemFor(Prefix.FILENUMBER);

        pane.getChildren().addAll(selectionPane,
                Views.titledPane(DefaultI18nContext.getInstance().i18n("Split settings"), splitOptions),
                Views.titledPane(DefaultI18nContext.getInstance().i18n("Output settings"), destinationPane),
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
        return new ImageView("split_by_size.png");
    }

    @Components({ SplitBySizeModule.class })
    public static class ModuleConfig {
        @Provides
        @Named(MODULE_ID + "field")
        public BrowsableOutputDirectoryField destinationDirectoryField() {
            BrowsableOutputDirectoryField field = new BrowsableOutputDirectoryField();
            field.setId(MODULE_ID + "field");
            return field;
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
            return new OpenButton(MODULE_ID, ModuleInputOutputType.MULTIPLE_PDF);
        }
    }
}
