/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 09/giu/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.split;

import java.util.Objects;
import java.util.function.Consumer;

import javafx.scene.control.RadioButton;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.support.TaskParametersBuildStep;
import org.pdfsam.support.validation.Validators;
import org.pdfsam.ui.commons.ValidableTextField;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.sejda.conversion.PageNumbersListAdapter;
import org.sejda.model.parameter.SplitByPagesParameters;

/**
 * Controller for a radio button driven text field that can accept a comma separated list of integer numbers.
 * 
 * @author Andrea Vacondio
 *
 */
class SplitAtRadioButtonDrivenTextFieldController implements TaskParametersBuildStep<SplitByPagesParameters> {

    private final RadioButton radio;
    private final ValidableTextField field;

    public SplitAtRadioButtonDrivenTextFieldController(RadioButton radio, ValidableTextField field) {
        Objects.requireNonNull(radio);
        Objects.requireNonNull(field);
        this.field = field;
        this.radio = radio;
        this.field.setValidator(Validators.newRegexMatchingString("^([0-9]+,?)+$"));
        this.field.setErrorMessage(DefaultI18nContext.getInstance().i18n("Invalid page numbers"));
    }

    public void apply(SplitByPagesParameters params, Consumer<String> onError) {
        if (radio.isSelected()) {
            this.field.validate();
            if (this.field.getValidationState() == ValidationState.INVALID) {
                onError.accept(DefaultI18nContext.getInstance().i18n("Invalid page numbers"));
            } else {
                params.addPages(new PageNumbersListAdapter(this.field.getText()).getPageNumbers());
            }
        }
    }

    public final boolean isSelected() {
        return radio.isSelected();
    }

}
