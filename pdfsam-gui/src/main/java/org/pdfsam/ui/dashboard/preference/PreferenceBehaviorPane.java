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

import javax.inject.Named;

import org.pdfsam.context.BooleanUserPreference;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.context.DefaultUserContext;
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
    PreferenceBehaviorPane() {
        I18nContext i18n = DefaultI18nContext.getInstance();

        PreferenceCheckBox checkForUpdates = new PreferenceCheckBox(BooleanUserPreference.CHECK_UPDATES,
                i18n.i18n("Check for updates at startup"), DefaultUserContext.getInstance().isCheckForUpdates());
        checkForUpdates.setTooltip(new Tooltip(i18n
                .i18n("Set whether new version availability should be checked on startup (restart needed)")));
        checkForUpdates.getStyleClass().add("spaced-vitem");

        PreferenceCheckBox playSounds = new PreferenceCheckBox(BooleanUserPreference.PLAY_SOUNDS,
                i18n.i18n("Play alert sounds"), DefaultUserContext.getInstance().isPlaySounds());
        playSounds.setTooltip(new Tooltip(i18n.i18n("Turn on or off alert sounds")));
        playSounds.getStyleClass().add("spaced-vitem");

        PreferenceCheckBox askConfirmation = new PreferenceCheckBox(BooleanUserPreference.ASK_OVERWRITE_CONFIRMATION,
                i18n.i18n("Ask for confirmation when the \"Overwrite\" checkbox is selected"), DefaultUserContext
                        .getInstance()
                        .isAskOverwriteConfirmation());
        askConfirmation.setTooltip(new Tooltip(i18n
                .i18n("Show a dialog box asking the user for confirmation when the \"overwrite\" is selected")));
        askConfirmation.getStyleClass().add("spaced-vitem");
        getChildren().addAll(checkForUpdates, playSounds, askConfirmation);
        getStyleClass().addAll(Style.CONTAINER.css());
    }

}
