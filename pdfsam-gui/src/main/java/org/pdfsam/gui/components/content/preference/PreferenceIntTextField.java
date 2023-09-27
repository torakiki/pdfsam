/*
 * This file is part of the PDF Split And Merge source code
 * Created on 01/set/2014
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
package org.pdfsam.gui.components.content.preference;

import org.pdfsam.core.context.ApplicationContext;
import org.pdfsam.core.context.IntegerPersistentProperty;
import org.pdfsam.core.support.validation.Validator;
import org.pdfsam.ui.components.commons.ValidableTextField;
import org.pdfsam.ui.components.support.FXValidationSupport;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.pdfsam.core.context.ApplicationContext.app;

/**
 * {@link ValidableTextField} setting an int preference on valid state
 *
 * @author Andrea Vacondio
 */
class PreferenceIntTextField extends ValidableTextField {

    private static final Logger LOG = LoggerFactory.getLogger(PreferenceIntTextField.class);

    PreferenceIntTextField(IntegerPersistentProperty property, Validator<String> validator) {
        this(property, app(), validator);
    }

    PreferenceIntTextField(IntegerPersistentProperty preference, ApplicationContext context,
            Validator<String> validator) {
        setValidator(validator);
        setEnableInvalidStyle(true);
        setOnEnterValidation(true);
        validProperty().addListener((o, oldVal, newVal) -> {
            if (newVal == FXValidationSupport.ValidationState.VALID) {
                context.persistentSettings().set(preference, Integer.parseInt(getText()));
                LOG.trace("Preference {} set to {}", preference, getText());
            }
        });
    }

}
