/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 13/giu/2012
 * Copyright 2012 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import javafx.scene.control.CheckBox;

import org.pdfsam.context.BooleanUserPreference;
import org.pdfsam.context.DefaultUserContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Checkbox updating the relative preference on item selection/deselection
 * 
 * @author Andrea Vacondio
 * 
 */
class PreferenceCheckBox extends CheckBox {

    private static final Logger LOG = LoggerFactory.getLogger(PreferenceCheckBox.class);
    private final BooleanUserPreference preference;

    PreferenceCheckBox(BooleanUserPreference preference, String label, boolean selected) {
        super(label);
        setSelected(selected);
        this.preference = preference;
        selectedProperty().addListener((ov, oldVal, newVal) -> {
            DefaultUserContext.getInstance().setBooleanPreference(PreferenceCheckBox.this.preference, newVal);
            LOG.trace("Preference {} set to {}", PreferenceCheckBox.this.preference, newVal);
        });
    }
}
