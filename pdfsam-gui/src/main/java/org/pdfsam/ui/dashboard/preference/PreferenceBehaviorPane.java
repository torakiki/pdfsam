/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30/ott/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.ui.dashboard.preference;

import javafx.scene.control.Tooltip;
import javafx.scene.layout.VBox;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.context.I18nContext;
import org.pdfsam.ui.support.Style;

/**
 * Preference pane displaying the behavior section
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
class PreferenceBehaviorPane extends VBox {

    @Inject
    public PreferenceBehaviorPane(@Named("checkForUpdates") PreferenceCheckBox checkForUpdates,
            @Named("playSounds") PreferenceCheckBox playSounds,
            @Named("askConfirmation") PreferenceCheckBox askConfirmation) {
        I18nContext i18n = DefaultI18nContext.getInstance();

        checkForUpdates.setTooltip(new Tooltip(i18n
                .i18n("Set whether new version availability should be checked on startup (restart needed)")));
        checkForUpdates.getStyleClass().add("spaced-vitem");

        playSounds.setTooltip(new Tooltip(i18n.i18n("Turn on or off alert sounds")));
        playSounds.getStyleClass().add("spaced-vitem");

        askConfirmation.setTooltip(new Tooltip(i18n
                .i18n("Show a dialog box asking the user for confirmation when the \"overwrite\" is selected")));
        askConfirmation.getStyleClass().add("spaced-vitem");
        getChildren().addAll(checkForUpdates, playSounds, askConfirmation);
        getStyleClass().addAll(Style.CONTAINER.css());
    }

}
