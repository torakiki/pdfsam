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

import java.util.function.Consumer;

import javafx.scene.control.RadioButton;
import javafx.scene.control.Tooltip;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.support.validation.Validators;
import org.pdfsam.ui.commons.ValidableTextField;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.sejda.conversion.PageNumbersListAdapter;
import org.sejda.model.parameter.SplitByPagesParameters;

/**
 * {@link RadioButton} driving a text field that can accept a comma separated list of integer numbers.
 * 
 * @author Andrea Vacondio
 *
 */
class SplitAfterRadioButton extends RadioButton implements SplitParamsCreator<SplitByPagesParameters> {

    private final ValidableTextField field = new ValidableTextField();

    public SplitAfterRadioButton() {
        super(DefaultI18nContext.getInstance().i18n("Split after the following page numbers"));
        field.setOnEnterValidation(true);
        field.setEnableInvalidStyle(true);
        field.setPromptText(DefaultI18nContext.getInstance().i18n("Page numbers to split at (n1,n2,n3..)"));
        setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("Split the document after the given page numbers")));
        field.setValidator(Validators.newRegexMatchingString("^([0-9]+,?)+$"));
        field.setErrorMessage(DefaultI18nContext.getInstance().i18n("Invalid page numbers"));
    }

    public SplitByPagesParameters createParams(Consumer<String> onError) {
        this.field.validate();
        if (this.field.getValidationState() == ValidationState.INVALID) {
            onError.accept(DefaultI18nContext.getInstance().i18n("Invalid page numbers"));
        } else {
            SplitByPagesParameters params = new SplitByPagesParameters();
            params.addPages(new PageNumbersListAdapter(this.field.getText()).getPageNumbers());
            return params;
        }
        return null;
    }

    ValidableTextField getField() {
        return field;
    }

}
