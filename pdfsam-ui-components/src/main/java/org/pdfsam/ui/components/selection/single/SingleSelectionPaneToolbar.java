/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 29 apr 2019
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.ui.components.selection.single;

import javafx.event.ActionEvent;
import javafx.scene.control.Button;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SplitMenuButton;
import javafx.scene.control.ToolBar;
import javafx.scene.control.Tooltip;
import javafx.stage.FileChooser;
import org.apache.commons.lang3.StringUtils;
import org.pdfsam.eventstudio.annotation.EventStation;
import org.pdfsam.model.tool.ClearToolRequest;
import org.pdfsam.model.tool.ToolBound;
import org.pdfsam.ui.components.selection.ToolbarButton;
import org.pdfsam.ui.components.support.Style;

import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Toolbar for the single selection panel
 *
 * @author Andrea Vacondio
 */
class SingleSelectionPaneToolbar extends ToolBar implements ToolBound {

    private String toolBinding = StringUtils.EMPTY;

    public SingleSelectionPaneToolbar(Button selectButton, String toolBinding) {
        this.toolBinding = defaultString(toolBinding);
        getItems().addAll(selectButton, new ClearButton(toolBinding));
        getStyleClass().add("selection-tool-bar");
    }

    /**
     * Button to request the load of a pdf document selected using a {@link FileChooser}
     *
     * @author Andrea Vacondio
     */
    public static class SelectButton extends ToolbarButton {

        public SelectButton(String ownerModule) {
            super(ownerModule);
            setTooltip(new Tooltip(i18n().tr("Select the PDF file")));
            setText(i18n().tr("_Select PDF"));
        }
    }

    static class ClearButton extends SplitMenuButton implements ToolBound {

        private String ownerModule = StringUtils.EMPTY;

        public ClearButton(String ownerModule) {
            setId("clear-button");
            this.ownerModule = defaultString(ownerModule);
            getStyleClass().addAll(Style.BUTTON.css());
            getStyleClass().addAll("pdfsam-split-button", "toolbar-splitbutton");
            setText(i18n().tr("_Clear"));
            setTooltip(new Tooltip(i18n().tr("Clear the selected PDF document")));
            setOnAction(this::clear);

            MenuItem clearAllSettings = new MenuItem();
            clearAllSettings.setText(i18n().tr("C_lear all settings"));
            clearAllSettings.setOnAction(this::clearAll);
            getItems().add(clearAllSettings);
        }

        public void clear(ActionEvent event) {
            eventStudio().broadcast(new ClearToolRequest(toolBinding(), false, false));
        }

        public void clearAll(ActionEvent event) {
            eventStudio().broadcast(new ClearToolRequest(toolBinding(), true, true));
        }

        @Override
        @EventStation
        public String toolBinding() {
            return ownerModule;
        }
    }

    @Override
    public String toolBinding() {
        return toolBinding;
    }

}
