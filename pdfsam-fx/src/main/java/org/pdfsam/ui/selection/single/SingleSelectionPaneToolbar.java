/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 29 apr 2019
 * Copyright 2017 by Sober Lemur S.a.s di Vacondio Andrea (info@pdfsam.org).
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
package org.pdfsam.ui.selection.single;

import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.module.ModuleOwned;
import org.pdfsam.ui.commons.ClearModuleEvent;
import org.pdfsam.ui.selection.ToolbarButton;

import javafx.event.ActionEvent;
import javafx.scene.control.Button;
import javafx.scene.control.ToolBar;
import javafx.scene.control.Tooltip;
import javafx.stage.FileChooser;

/**
 * Toolbar for the single selection panel
 * 
 * @author Andrea Vacondio
 *
 */
class SingleSelectionPaneToolbar extends ToolBar implements ModuleOwned {

    private String ownerModule = StringUtils.EMPTY;

    public SingleSelectionPaneToolbar(Button selectButton, String ownerModule) {
        this.ownerModule = defaultString(ownerModule);
        getItems().addAll(selectButton, new ClearButton(ownerModule));
        getStyleClass().add("selection-tool-bar");
    }

    /**
     * Button to request the load of a pdf document selected using a {@link FileChooser}
     * 
     * @author Andrea Vacondio
     * 
     */
    public static class SelectButton extends ToolbarButton {

        public SelectButton(String ownerModule) {
            super(ownerModule);
            setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("Select the PDF file")));
            setText(DefaultI18nContext.getInstance().i18n("_Select PDF"));
        }
    }

    static class ClearButton extends ToolbarButton {

        public ClearButton(String ownerModule) {
            super(ownerModule);
            setText(DefaultI18nContext.getInstance().i18n("_Clear"));
            setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("Clear all settings")));
            setOnAction(this::clearAll);
            eventStudio().addAnnotatedListeners(this);
        }

        public void clearAll(ActionEvent event) {
            eventStudio().broadcast(new ClearModuleEvent(true), getOwnerModule());
        }
    }

    @Override
    public String getOwnerModule() {
        return ownerModule;
    }

}
