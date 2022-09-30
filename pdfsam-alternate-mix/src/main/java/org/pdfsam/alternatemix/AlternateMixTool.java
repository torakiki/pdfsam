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
package org.pdfsam.alternatemix;

import jakarta.inject.Named;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.image.ImageView;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import org.pdfsam.core.context.UserContext;
import org.pdfsam.eventstudio.annotation.EventListener;
import org.pdfsam.eventstudio.annotation.EventStation;
import org.pdfsam.i18n.I18nContext;
import org.pdfsam.injector.Auto;
import org.pdfsam.injector.Components;
import org.pdfsam.injector.Provides;
import org.pdfsam.tool.ToolCategory;
import org.pdfsam.tool.ToolDescriptor;
import org.pdfsam.tool.ToolInputOutputType;
import org.pdfsam.tool.ToolPriority;
import org.pdfsam.ui.commons.ClearModuleEvent;
import org.pdfsam.ui.components.io.BrowsablePdfOutputField;
import org.pdfsam.ui.components.io.PdfDestinationPane;
import org.pdfsam.ui.components.tool.BaseTaskExecutionTool;
import org.pdfsam.ui.components.tool.Footer;
import org.pdfsam.ui.components.tool.OpenButton;
import org.pdfsam.ui.components.tool.RunButton;
import org.pdfsam.ui.components.support.Views;

import javax.inject.Inject;
import java.util.Map;
import java.util.function.Consumer;

import static org.pdfsam.tool.ToolDescriptorBuilder.builder;

/**
 * Alternate mix module to let the user merge two pdf documents taking pages alternately in straight or reverse order.
 *
 * @author Andrea Vacondio
 */
@Auto
public class AlternateMixTool extends BaseTaskExecutionTool {

    private static final String MODULE_ID = "alternatemix";

    private AlternateMixSelectionPane selectionPane = new AlternateMixSelectionPane(MODULE_ID);
    private BrowsablePdfOutputField destinationFileField;
    private PdfDestinationPane destinationPane;
    private ToolDescriptor descriptor = builder().category(ToolCategory.MERGE)
            .inputTypes(ToolInputOutputType.MULTIPLE_PDF).name(i18n().tr("Alternate Mix"))
            .description(I18nContext.getInstance()
                    .i18n("Merge two or more PDF documents taking pages alternately in natural or reverse order."))
            .priority(ToolPriority.DEFAULT.getPriority()).supportURL("https://pdfsam.org/mix-pdf/")
            .build();

    @Inject
    public AlternateMixTool(@Named(MODULE_ID + "field") BrowsablePdfOutputField destinationFileField,
            @Named(MODULE_ID + "pane") PdfDestinationPane destinationPane, @Named(MODULE_ID + "footer") Footer footer) {
        super(footer);
        this.destinationFileField = destinationFileField;
        this.destinationPane = destinationPane;
        initModuleSettingsPanel(settingPanel());
    }

    @Override
    public ToolDescriptor descriptor() {
        return descriptor;
    }

    @Override
    public void onSaveWorkspace(Map<String, String> data) {
        selectionPane.saveStateTo(data);
        destinationFileField.saveStateTo(data);
        destinationPane.saveStateTo(data);
    }

    @Override
    public void onLoadWorkspace(Map<String, String> data) {
        // backwards comp when alternate mix had 2 inputs
        if (data.containsKey("firstDocumentMixinput")) {
            data.put("input.0", data.get("firstDocumentMixinput"));
            data.put("input.password.0", data.get("firstDocumentMixinputinput.password"));
            data.put("input.step.0", data.get("firstStep"));
            data.put("input.reverse.0", data.get("reverseFirst"));
            data.put("input.size", "1");
            if (data.containsKey("secondDocumentMixinput")) {
                data.put("input.1", data.get("secondDocumentMixinput"));
                data.put("input.password.1", data.get("secondDocumentMixinput.password"));
                data.put("input.step.1", data.get("secondStep"));
                data.put("input.reverse.1", data.get("reverseSecond"));
                data.put("input.size", "2");
            }
        }
        selectionPane.restoreStateFrom(data);
        destinationFileField.restoreStateFrom(data);
        destinationPane.restoreStateFrom(data);
    }

    private VBox settingPanel() {
        VBox pane = new VBox();
        pane.setAlignment(Pos.TOP_CENTER);
        VBox.setVgrow(selectionPane, Priority.ALWAYS);

        pane.getChildren().addAll(selectionPane, Views.titledPane(i18n().tr("Destination file"), destinationPane));
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
            destinationPane.resetView();
        }
    }

    @Override
    public Node graphic() {
        return new ImageView("alternate_mix.png");
    }

    @Override
    protected AlternateMixParametersBuilder getBuilder(Consumer<String> onError) {
        AlternateMixParametersBuilder builder = new AlternateMixParametersBuilder();
        selectionPane.apply(builder, onError);
        destinationFileField.apply(builder, onError);
        destinationPane.apply(builder, onError);
        return builder;
    }

    @Components({ AlternateMixTool.class })
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
            return new OpenButton(MODULE_ID, ToolInputOutputType.SINGLE_PDF);
        }

    }
}
