/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 23/ott/2013
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
package org.pdfsam.ui.support;

import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

import org.pdfsam.support.validation.Validator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javafx.beans.property.ReadOnlyObjectProperty;
import javafx.beans.property.ReadOnlyObjectWrapper;

/**
 * Support class allowing validation of a value of the given type and allowing binding to the state of the validation. By default it behaves as always valid but a custom
 * {@link Validator} can be set.
 * 
 * @author Andrea Vacondio
 * @param <T>
 *            the type to validate
 */
public class FXValidationSupport<T> {

    private static final Logger LOG = LoggerFactory.getLogger(FXValidationSupport.class);

    private ReadOnlyObjectWrapper<ValidationState> validationState = new ReadOnlyObjectWrapper<>(
            ValidationState.NOT_VALIDATED);
    private Validator<T> validator = new Validator<>() {
        @Override
        public boolean isValid(T input) {
            return true;
        }
    };

    public void validate(T value) {
        LOG.trace("Validating {}", value);
        if (validator.isValid(value)) {
            validationState.set(ValidationState.VALID);
        } else {
            validationState.set(ValidationState.INVALID);
        }
    }

    public void setValidator(Validator<T> validator) {
        requireNotNullArg(validator, "Validator cannot be null");
        this.validator = validator;
        makeNotValidated();
    }

    public void makeNotValidated() {
        LOG.trace("Making state {}", ValidationState.NOT_VALIDATED);
        validationState.set(ValidationState.NOT_VALIDATED);
    }

    public final ReadOnlyObjectProperty<ValidationState> validationStateProperty() {
        return validationState.getReadOnlyProperty();
    }

    /**
     * Possible validation states
     * 
     * @author Andrea Vacondio
     * 
     */
    public static enum ValidationState {
        VALID,
        INVALID,
        NOT_VALIDATED;
    }
}
