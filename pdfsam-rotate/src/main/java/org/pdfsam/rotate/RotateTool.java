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
package org.pdfsam.rotate;

import jakarta.inject.Named;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.TitledPane;
import javafx.scene.image.ImageView;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import org.pdfsam.context.UserContext;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.eventstudio.annotation.EventStation;
import org.pdfsam.i18n.I18nContext;
import org.pdfsam.injector.Auto;
import org.pdfsam.injector.Components;
import org.pdfsam.injector.Provides;
import org.pdfsam.module.ToolCategory;
import org.pdfsam.module.ToolDescriptor;
import org.pdfsam.module.ToolInputOutputType;
import org.pdfsam.module.ToolPriority;
import org.pdfsam.ui.commons.ClearModuleEvent;
import org.pdfsam.ui.io.BrowsableOutputDirectoryField;
import org.pdfsam.ui.io.PdfDestinationPane;
import org.pdfsam.ui.module.BaseTaskExecutionTool;
import org.pdfsam.ui.module.Footer;
import org.pdfsam.ui.module.OpenButton;
import org.pdfsam.ui.module.RunButton;
import org.pdfsam.ui.prefix.PrefixPane;
import org.pdfsam.ui.support.Views;
import org.sejda.model.prefix.Prefix;

import javax.inject.Inject;
import java.util.Map;
import java.util.function.Consumer;

import static org.pdfsam.module.ToolDescriptorBuilder.builder;

/**
 * Rotate module to let the user rotate multiple PDF documents
 *
 * @author Andrea Vacondio
 */
@Auto
public class RotateTool extends BaseTaskExecutionTool {

    private static final String MODULE_ID = "rotate";

    private RotateSelectionPane selectionPane = new RotateSelectionPane(MODULE_ID);
    private RotateOptionsPane rotateOptions = new RotateOptionsPane();
    private BrowsableOutputDirectoryField destinationDirectoryField;
    private PdfDestinationPane destinationPane;
    private PrefixPane prefix;
    private ToolDescriptor descriptor = builder().category(ToolCategory.OTHER)
            .inputTypes(ToolInputOutputType.MULTIPLE_PDF, ToolInputOutputType.SINGLE_PDF)
            .name(I18nContext.getInstance().i18n("Rotate"))
            .description(I18nContext.getInstance().i18n("Rotate the pages of multiple PDF documents."))
            .priority(ToolPriority.DEFAULT.getPriority()).supportURL("https://pdfsam.org/rotate-pdf/")
            .build();

    @Inject
    public RotateTool(@Named(MODULE_ID + "field") BrowsableOutputDirectoryField destinationDirectoryField,
            @Named(MODULE_ID + "pane") PdfDestinationPane destinationPane, @Named(MODULE_ID + "footer") Footer footer,
            @Named(MODULE_ID + "prefix") PrefixPane prefix) {
        super(footer);
        this.destinationDirectoryField = destinationDirectoryField;
        this.destinationPane = destinationPane;
        this.prefix = prefix;
        initModuleSettingsPanel(settingPanel());
    }

    @Override
    public ToolDescriptor descriptor() {
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

    private VBox settingPanel() {
        VBox pane = new VBox();
        pane.setAlignment(Pos.TOP_CENTER);
        VBox.setVgrow(selectionPane, Priority.ALWAYS);

        TitledPane prefixTitled = Views.titledPane(I18nContext.getInstance().i18n("File names settings"),
                prefix);
        prefix.addMenuItemFor(Prefix.FILENUMBER);
        prefix.addMenuItemFor("[TOTAL_FILESNUMBER]");

        TitledPane options = Views.titledPane(I18nContext.getInstance().i18n("Rotate settings"), rotateOptions);

        pane.getChildren().addAll(selectionPane, options,
                Views.titledPane(I18nContext.getInstance().i18n("Output settings"), destinationPane),
                prefixTitled);
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
            rotateOptions.resetView();
            prefix.resetView();
            destinationPane.resetView();
        }
    }

    @Override
    public Node graphic() {
        return new ImageView("rotate.png");
    }

    @Components({ RotateTool.class })
    public static class ModuleConfig {
        @Provides
        @Named(MODULE_ID + "field")
        public BrowsableOutputDirectoryField destinationDirectoryField() {
            return new BrowsableOutputDirectoryField();
        }

        @Provides
        @Named(MODULE_ID + "pane")
        public PdfDestinationPane destinationPane(@Named(MODULE_ID + "field") BrowsableOutputDirectoryField outputField,
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
            return new OpenButton(MODULE_ID, ToolInputOutputType.MULTIPLE_PDF);
        }

        @Provides
        @Named(MODULE_ID + "prefix")
        public PrefixPane prefixPane(UserContext userContext) {
            return new PrefixPane(MODULE_ID, userContext);
        }
    }

}
