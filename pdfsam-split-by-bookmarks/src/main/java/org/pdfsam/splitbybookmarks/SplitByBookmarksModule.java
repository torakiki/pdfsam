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

import java.util.function.Consumer;

import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.TitledPane;
import javafx.scene.image.ImageView;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.module.ModuleCategory;
import org.pdfsam.module.ModuleDescriptor;
import org.pdfsam.module.ModulePriority;
import org.pdfsam.module.PdfsamModule;
import org.pdfsam.module.RequiredPdfData;
import org.pdfsam.ui.io.BrowsableDirectoryField;
import org.pdfsam.ui.io.PdfDestinationPane;
import org.pdfsam.ui.module.BaseTaskExecutionModule;
import org.pdfsam.ui.prefix.PrefixPane;
import org.pdfsam.ui.selection.single.SingleSelectionPane;
import org.pdfsam.ui.support.Views;
import org.sejda.model.parameter.SplitByGoToActionLevelParameters;
import org.sejda.model.parameter.base.TaskParameters;
import org.sejda.model.prefix.Prefix;

/**
 * Merge module to let the user merge together multiple pdf documents
 * 
 * @author Andrea Vacondio
 *
 */
@PdfsamModule
public class SplitByBookmarksModule extends BaseTaskExecutionModule {

    private static final String SPLIT_MODULE_ID = "split.bybookmarks";

    private SingleSelectionPane<SplitByGoToActionLevelParameters> selectionPane;
    private BrowsableDirectoryField destinationDirectoryField = new BrowsableDirectoryField(false);
    private SplitOptionsPane splitOptions = new SplitOptionsPane();
    private PdfDestinationPane destinationPane;
    private PrefixPane prefix = new PrefixPane();
    private ModuleDescriptor descriptor = builder()
            .category(ModuleCategory.SPLIT)
            .name(DefaultI18nContext.getInstance().i18n("Split by bookmarks"))
            .description(
                    DefaultI18nContext.getInstance().i18n(
                            "Split a pdf document at bookmarked pages by specifying a bookmark level."))
            .priority(ModulePriority.DEFAULT.getPriority()).supportURL("http://www.pdfsam.org/split-by-bookmark")
            .build();

    public SplitByBookmarksModule() {
        this.selectionPane = new SingleSelectionPane<>(id());
        this.destinationPane = new PdfDestinationPane(destinationDirectoryField, id());
        this.destinationPane.enableSameAsSourceItem();
        this.selectionPane.addOnLoaded(d -> splitOptions.setMaxBookmarkLevel(d.getMaxGoToActionDepth()));
    }

    @Override
    public ModuleDescriptor descriptor() {
        return descriptor;
    }

    @Override
    protected TaskParameters buildParameters(Consumer<String> onError) {
        SplitByGoToActionLevelParameters params = splitOptions.createParams(onError);
        if (params != null) {
            selectionPane.apply(params, onError);
            splitOptions.apply(params, onError);
            destinationDirectoryField.apply(params, onError);
            destinationPane.apply(params, onError);
            prefix.apply(params, onError);
        }
        return params;
    }

    @Override
    protected Pane getInnerPanel() {
        VBox pane = new VBox(5);
        pane.setAlignment(Pos.TOP_CENTER);

        TitledPane prefixTitled = Views
                .titledPane(DefaultI18nContext.getInstance().i18n("File names settings"), prefix);
        prefix.addMenuItemFor(Prefix.CURRENTPAGE);
        prefix.addMenuItemFor(Prefix.FILENUMBER);

        pane.getChildren().addAll(selectionPane,
                Views.titledPane(DefaultI18nContext.getInstance().i18n("Split settings"), splitOptions),
                Views.titledPane(DefaultI18nContext.getInstance().i18n("Destination directory"), destinationPane),
                prefixTitled);
        return pane;
    }

    @Override
    public String id() {
        return SPLIT_MODULE_ID;
    }

    public RequiredPdfData[] requires() {
        return new RequiredPdfData[] { RequiredPdfData.DEFAULT, RequiredPdfData.BOOMARKS };
    }

    public Node graphic() {
        return new ImageView("split_by_bookmarks.png");
    }
}
