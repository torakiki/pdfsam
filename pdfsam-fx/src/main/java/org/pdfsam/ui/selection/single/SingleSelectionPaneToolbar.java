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
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.module.ModuleOwned;
import org.pdfsam.ui.commons.ClearModuleEvent;
import org.pdfsam.ui.module.ModuleOwnedButton;

import javafx.event.ActionEvent;
import javafx.scene.control.ToolBar;

/**
 * Toolbar for the single selection panel
 * 
 * @author Andrea Vacondio
 *
 */
class SingleSelectionPaneToolbar extends ToolBar implements ModuleOwned {

    private String ownerModule = StringUtils.EMPTY;

    public SingleSelectionPaneToolbar(String ownerModule) {
        this.ownerModule = defaultString(ownerModule);
        getItems().addAll(new ClearButton(ownerModule));
        getStyleClass().add("selection-tool-bar");
    }

    static class ClearButton extends ModuleOwnedButton {

        public ClearButton(String ownerModule) {
            super(ownerModule);
            setText(DefaultI18nContext.getInstance().i18n("C_lear all settings"));
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
