/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 29/ott/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.gui.preference;

import static org.pdfsam.support.RequireUtils.requireNotNull;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;

import org.pdfsam.context.DefaultUserContext;
import org.pdfsam.context.StringUserPreference;
import org.pdfsam.ui.BrowsableField;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * BrowsableField that sets a {@link StringUserPreference} when the input text is valid.
 * 
 * @author Andrea Vacondio
 * 
 */
class PreferenceBrowsableField extends BrowsableField {
    private static final Logger LOG = LoggerFactory.getLogger(PreferenceBrowsableField.class);
    private StringUserPreference preference;

    PreferenceBrowsableField(StringUserPreference preference) {
        requireNotNull(preference, "Preference cannot be null");
        this.preference = preference;
        getTextField().validProperty().addListener(new PreferenceSetter());
    }

    /**
     * Listens for a value change in the validation state and when the state is {@link ValidationState#VALID} it sets the corresponding preference
     * 
     * @author Andrea Vacondio
     * 
     */
    private class PreferenceSetter implements ChangeListener<ValidationState> {
        @Override
        public void changed(ObservableValue<? extends ValidationState> observable, ValidationState oldValue,
                ValidationState newValue) {
            if (newValue == ValidationState.VALID) {
                DefaultUserContext.getInstance().setStringPreference(preference, getTextField().getText());
                LOG.trace("Preference {} set to {}", preference, getTextField().getText());
            }
        }
    }
}
