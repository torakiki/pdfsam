/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 12/giu/2014
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

import java.util.function.Consumer;

import javafx.scene.control.RadioButton;
import javafx.scene.control.Tooltip;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.support.validation.Validators;
import org.pdfsam.ui.commons.ValidableTextField;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.sejda.model.parameter.SplitByEveryXPagesParameters;

/**
 * {@link RadioButton} driving a text field that can accept an integer number.
 * 
 * @author Andrea Vacondio
 *
 */
public class SplitByEveryRadioButton extends RadioButton implements
        SplitParamsCreator<SplitByEveryXPagesParameters> {

    private final ValidableTextField field = new ValidableTextField();

    public SplitByEveryRadioButton() {
        super(DefaultI18nContext.getInstance().i18n("Split by every \"n\" pages"));
        field.setOnEnterValidation(true);
        field.setEnableInvalidStyle(true);
        field.setPromptText(DefaultI18nContext.getInstance().i18n("Number of pages"));
        setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n(
                "Splits the pdf every \"n\" pages creating documents of \"n\" pages each")));
        field.setValidator(Validators.newRegexMatchingString("^(\\d)+$"));
        field.setErrorMessage(DefaultI18nContext.getInstance().i18n("Invalid number of pages"));
    }

    public SplitByEveryXPagesParameters createParams(Consumer<String> onError) {
        this.field.validate();
        if (this.field.getValidationState() == ValidationState.INVALID) {
            onError.accept(DefaultI18nContext.getInstance().i18n("Invalid number of pages"));
        } else {
            return new SplitByEveryXPagesParameters(Integer.parseInt(this.field.getText()));
        }
        return null;
    }

    ValidableTextField getField() {
        return field;
    }

}
