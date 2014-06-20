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

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.module.ModuleCategory;
import org.pdfsam.module.ModuleDescriptor;
import org.pdfsam.module.ModulePriority;
import org.pdfsam.module.PdfsamModule;
import org.pdfsam.ui.io.BrowsablePdfOutputField;
import org.pdfsam.ui.io.PdfDestinationPane;
import org.pdfsam.ui.module.BaseTaskExecutionModule;
import org.pdfsam.ui.support.Views;
import org.sejda.model.parameter.MergeParameters;
import org.sejda.model.parameter.base.TaskParameters;

/**
 * Merge module to let the user merge together multiple pdf documents
 * 
 * @author Andrea Vacondio
 *
 */
@PdfsamModule
public class MergeModule extends BaseTaskExecutionModule {

    private static final String MERGE_MODULE_ID = "merge";

    private MergeSelectionPane selectionPane;
    private MergeOptionsPane mergeOptions = new MergeOptionsPane();
    private BrowsablePdfOutputField destinationFileField = new BrowsablePdfOutputField();
    private PdfDestinationPane destinationPane;
    private ModuleDescriptor descriptor = builder()
            .category(ModuleCategory.MERGE)
            .name(DefaultI18nContext.getInstance().i18n("Merge"))
            .description(
                    DefaultI18nContext.getInstance().i18n(
                            "Merge together multiple pdf documents or subsections of them."))
            .priority(ModulePriority.HIGH.getPriority()).supportURL("http://www.pdfsam.org/merge").build();

    public MergeModule() {
        this.selectionPane = new MergeSelectionPane(id());
        this.destinationFileField.enforceValidation(false, false);
        this.destinationPane = new PdfDestinationPane(destinationFileField, id());
    }

    @Override
    public ModuleDescriptor descriptor() {
        return descriptor;
    }

    @Override
    protected TaskParameters buildParameters(Consumer<String> onError) {
        MergeParameters params = new MergeParameters();
        selectionPane.apply(params, onError);
        mergeOptions.apply(params, onError);
        destinationFileField.apply(params, onError);
        destinationPane.apply(params, onError);
        return params;
    }

    @Override
    protected Pane getInnerPanel() {
        VBox pane = new VBox(5);
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
        return MERGE_MODULE_ID;
    }

    public Node graphic() {
        return new ImageView("merge.png");
    }
}
