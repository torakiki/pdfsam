/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30/ott/2013
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
package org.pdfsam.gui.components.dashboard.preference;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import org.pdfsam.core.context.ApplicationContext;
import org.pdfsam.core.context.StringPersistentProperty;
import org.pdfsam.ui.components.commons.ValidableTextField;
import org.pdfsam.ui.components.support.FXValidationSupport;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

/**
 * @author Andrea Vacondio
 *
 */
class PreferenceSetterOnValidState implements ChangeListener<FXValidationSupport.ValidationState> {

    private static final Logger LOG = LoggerFactory.getLogger(PreferenceBrowsableFileField.class);

    private final StringPersistentProperty property;
    private final ValidableTextField textField;
    private final ApplicationContext context;

    PreferenceSetterOnValidState(StringPersistentProperty property, ValidableTextField textField,
            ApplicationContext context) {
        requireNotNullArg(property, "Preference cannot be null");
        requireNotNullArg(textField, "TextField cannot be null");
        this.textField = textField;
        this.property = property;
        this.context = context;
    }

    @Override
    public void changed(ObservableValue<? extends FXValidationSupport.ValidationState> observable,
            FXValidationSupport.ValidationState oldValue, FXValidationSupport.ValidationState newValue) {
        if (newValue == FXValidationSupport.ValidationState.VALID) {
            context.persistentSettings().set(property, textField.getText());
            LOG.trace("Preference {} set to {}", property, textField.getText());
        }
    }

}
