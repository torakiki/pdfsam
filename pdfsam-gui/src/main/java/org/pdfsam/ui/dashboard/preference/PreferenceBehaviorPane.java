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

import javafx.scene.layout.VBox;

import javax.inject.Inject;
import javax.inject.Named;

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
            @Named("playSounds") PreferenceCheckBox playSounds) {
        super(checkForUpdates, playSounds);
        getStyleClass().addAll(Style.CONTAINER.css());
    }

}
