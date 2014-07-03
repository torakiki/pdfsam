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
import org.pdfsam.ui.io.BrowsableOutputDirectoryField;
import org.pdfsam.ui.io.PdfDestinationPane;
import org.pdfsam.ui.module.BaseTaskExecutionModule;
import org.pdfsam.ui.prefix.PrefixPane;
import org.pdfsam.ui.support.Style;
import org.pdfsam.ui.support.Views;
import org.sejda.model.prefix.Prefix;

/**
 * Rotate module to let the user rotate multiple PDF documents
 * 
 * @author Andrea Vacondio
 *
 */
@PdfsamModule
public class RotateModule extends BaseTaskExecutionModule {

    private static final String ROTATE_MODULE_ID = "rotate";

    private RotateSelectionPane selectionPane;
    private RotateOptionsPane rotateOptions = new RotateOptionsPane();
    private BrowsableOutputDirectoryField destinationFileField = new BrowsableOutputDirectoryField(false);
    private PdfDestinationPane destinationPane;
    private PrefixPane prefix = new PrefixPane();
    private ModuleDescriptor descriptor = builder().category(ModuleCategory.OTHER)
            .name(DefaultI18nContext.getInstance().i18n("Rotate"))
            .description(DefaultI18nContext.getInstance().i18n("Rotate the pages of multiple PDF documents."))
            .priority(ModulePriority.DEFAULT.getPriority()).supportURL("http://www.pdfsam.org/pdf-rotate").build();

    public RotateModule() {
        this.selectionPane = new RotateSelectionPane(id());
        this.destinationPane = new PdfDestinationPane(destinationFileField, id());
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
        destinationFileField.apply(builder, onError);
        destinationPane.apply(builder, onError);
        prefix.apply(builder, onError);
        return builder;
    }

    @Override
    protected Pane getInnerPanel() {
        VBox pane = new VBox(Style.DEFAULT_SPACING);
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
        return ROTATE_MODULE_ID;
    }

    public Node graphic() {
        return new ImageView("rotate.png");
    }
}
