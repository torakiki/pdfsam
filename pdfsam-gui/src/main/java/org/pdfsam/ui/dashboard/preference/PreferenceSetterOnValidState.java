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

import static org.pdfsam.support.RequireUtils.requireNotNull;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;

import org.pdfsam.context.StringUserPreference;
import org.pdfsam.context.UserContext;
import org.pdfsam.ui.commons.ValidableTextField;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Andrea Vacondio
 * 
 */
class PreferenceSetterOnValidState implements ChangeListener<ValidationState> {

    private static final Logger LOG = LoggerFactory.getLogger(PreferenceBrowsableFileField.class);

    private StringUserPreference preference;
    private ValidableTextField textField;
    private UserContext userContext;

    PreferenceSetterOnValidState(StringUserPreference preference, ValidableTextField textField, UserContext userContext) {
        requireNotNull(preference, "Preference cannot be null");
        requireNotNull(textField, "TextField cannot be null");
        this.textField = textField;
        this.preference = preference;
        this.userContext = userContext;
    }

    @Override
    public void changed(ObservableValue<? extends ValidationState> observable, ValidationState oldValue,
            ValidationState newValue) {
        if (newValue == ValidationState.VALID) {
            userContext.setStringPreference(preference, textField.getText());
            LOG.trace("Preference {} set to {}", preference, textField.getText());
        }
    }

}
