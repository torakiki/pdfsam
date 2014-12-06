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
package org.pdfsam.splitbybookmarks;

import static org.pdfsam.module.ModuleDescriptorBuilder.builder;

import java.util.Map;
import java.util.function.Consumer;

import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.TitledPane;
import javafx.scene.image.ImageView;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;

import javax.inject.Inject;
import javax.inject.Named;

import org.apache.commons.lang3.builder.Builder;
import org.pdfsam.context.UserContext;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.module.ModuleCategory;
import org.pdfsam.module.ModuleDescriptor;
import org.pdfsam.module.ModulePriority;
import org.pdfsam.module.PdfsamModule;
import org.pdfsam.module.RequiredPdfData;
import org.pdfsam.ui.io.BrowsableOutputDirectoryField;
import org.pdfsam.ui.io.PdfDestinationPane;
import org.pdfsam.ui.module.BaseTaskExecutionModule;
import org.pdfsam.ui.prefix.PrefixPane;
import org.pdfsam.ui.selection.single.TaskParametersBuilderSingleSelectionPane;
import org.pdfsam.ui.support.Views;
import org.pdfsam.ui.workspace.SaveWorkspaceEvent;
import org.sejda.eventstudio.annotation.EventStation;
import org.sejda.model.parameter.SplitByGoToActionLevelParameters;
import org.sejda.model.prefix.Prefix;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Merge module to let the user merge together multiple pdf documents
 * 
 * @author Andrea Vacondio
 *
 */
@PdfsamModule
public class SplitByBookmarksModule extends BaseTaskExecutionModule {

    private static final String MODULE_ID = "split.bybookmarks";

    private TaskParametersBuilderSingleSelectionPane selectionPane;
    private BrowsableOutputDirectoryField destinationDirectoryField;
    private PdfDestinationPane destinationPane;
    private SplitOptionsPane splitOptions = new SplitOptionsPane();
    private PrefixPane prefix = new PrefixPane();
    private ModuleDescriptor descriptor = builder()
            .category(ModuleCategory.SPLIT)
            .name(DefaultI18nContext.getInstance().i18n("Split by bookmarks"))
            .description(
                    DefaultI18nContext.getInstance().i18n(
                            "Split a pdf document at bookmarked pages by specifying a bookmark level."))
            .priority(ModulePriority.DEFAULT.getPriority()).supportURL("http://www.pdfsam.org/pdf-split-by-bookmark")
            .build();

    @Inject
    public SplitByBookmarksModule(@Named(MODULE_ID + "field") BrowsableOutputDirectoryField destinationDirectoryField,
            @Named(MODULE_ID + "pane") PdfDestinationPane destinationPane) {
        this.destinationDirectoryField = destinationDirectoryField;
        this.destinationPane = destinationPane;
        this.selectionPane = new TaskParametersBuilderSingleSelectionPane(id());
        this.selectionPane.setPromptText(DefaultI18nContext.getInstance().i18n(
                "Select or drag and drop the PDF you want to split"));
        this.selectionPane.addOnLoaded(d -> splitOptions.setMaxBookmarkLevel(d.getMaxGoToActionDepth()));

    }

    public void onSaveWorkspace(SaveWorkspaceEvent event) {
        Map<String, String> data = event.getDataForModule(MODULE_ID);
        splitOptions.saveStateTo(data);
        destinationDirectoryField.saveStateTo(data);
        destinationPane.saveStateTo(data);
        prefix.saveStateTo(data);
    }

    @Override
    public ModuleDescriptor descriptor() {
        return descriptor;
    }

    @Override
    protected Builder<SplitByGoToActionLevelParameters> getBuilder(Consumer<String> onError) {
        SplitByGoToActionLevelParametersBuilder builder = new SplitByGoToActionLevelParametersBuilder();
        splitOptions.apply(builder, onError);
        selectionPane.apply(builder, onError);
        destinationDirectoryField.apply(builder, onError);
        destinationPane.apply(builder, onError);
        prefix.apply(builder, onError);
        return builder;
    }

    @Override
    protected Pane getInnerPanel() {
        VBox pane = new VBox();
        pane.setAlignment(Pos.TOP_CENTER);

        TitledPane prefixTitled = Views
                .titledPane(DefaultI18nContext.getInstance().i18n("File names settings"), prefix);
        prefix.addMenuItemFor(Prefix.CURRENTPAGE);
        prefix.addMenuItemFor(Prefix.FILENUMBER);
        prefix.addMenuItemFor(Prefix.BOOKMARK);
        prefix.addMenuItemFor(Prefix.BOOKMARK_STRICT);

        pane.getChildren().addAll(selectionPane,
                Views.titledPane(DefaultI18nContext.getInstance().i18n("Split settings"), splitOptions),
                Views.titledPane(DefaultI18nContext.getInstance().i18n("Destination directory"), destinationPane),
                prefixTitled);
        return pane;
    }

    @Override
    @EventStation
    public String id() {
        return MODULE_ID;
    }

    public RequiredPdfData[] requires() {
        return new RequiredPdfData[] { RequiredPdfData.DEFAULT, RequiredPdfData.BOOMARKS };
    }

    public Node graphic() {
        return new ImageView("split_by_bookmarks.png");
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
            PdfDestinationPane panel = new PdfDestinationPane(outputField, MODULE_ID, userContext);
            panel.enableSameAsSourceItem();
            return panel;
        }
    }
}
