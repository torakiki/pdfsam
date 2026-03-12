/*
 * This file is part of the PDF Split And Merge source code
 * Created on 12/giu/2014
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
package org.pdfsam.tools.split;

import javafx.scene.control.RadioButton;
import org.pdfsam.core.support.params.SplitParametersBuilder;
import org.pdfsam.core.support.validation.Validators;
import org.pdfsam.model.ui.ResettableView;
import org.pdfsam.model.ui.workspace.RestorableView;
import org.pdfsam.ui.components.commons.ValidableTextField;
import org.pdfsam.ui.components.support.FXValidationSupport.ValidationState;
import org.sejda.model.parameter.SplitByEveryXPagesParameters;

import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * {@link RadioButton} driving a text field that can accept an integer number.
 * 
 * @author Andrea Vacondio
 *
 */
public class SplitByEveryRadioButton extends RadioButton
        implements SplitParametersBuilderCreator, RestorableView, ResettableView {

    private final ValidableTextField field;

    public SplitByEveryRadioButton(ValidableTextField field) {
        super(i18n().tr("Split by every \"n\" pages"));
        this.setAccessibleHelp(i18n().tr("Select to split the document every 'n' pages"));
        this.field = field;
        this.field.setOnEnterValidation(true);
        this.field.setEnableInvalidStyle(true);
        this.field.setPromptText(i18n().tr("Number of pages"));
        this.field.setValidator(Validators.positiveInteger());
        this.field.setErrorMessage(i18n().tr("Invalid number of pages"));
        this.field.setAccessibleText(i18n().tr("Number of pages for split"));
        this.field.setAccessibleHelp(i18n().tr("Enter a positive number of pages"));
    }

    @Override
    public SplitByEveryXPagesParametersBuilder getBuilder(Consumer<String> onError) {
        this.field.validate();
        if (this.field.getValidationState() == ValidationState.VALID) {
            return new SplitByEveryXPagesParametersBuilder(Integer.parseInt(this.field.getText()));
        }
        onError.accept(i18n().tr("Invalid number of pages"));
        return null;
    }

    @Override
    public void saveStateTo(Map<String, String> data) {
        if (isSelected()) {
            data.put("splitByEvery", Boolean.TRUE.toString());
        }
        data.put("splitByEvery.field", defaultString(field.getText()));
    }

    @Override
    public void restoreStateFrom(Map<String, String> data) {
        Optional.ofNullable(data.get("splitByEvery")).map(Boolean::valueOf).ifPresent(this::setSelected);
        field.setText(Optional.ofNullable(data.get("splitByEvery.field")).orElse(EMPTY));
    }

    void setMaxPages(Integer value) {
        if (value > 1) {
            this.field.setValidator(Validators.positiveIntRange(1, value - 1));
        }
    }

    @Override
    public void resetView() {
        field.clear();
    }

    /**
     * Builder for the {@link SplitByEveryXPagesParameters}
     * 
     * @author Andrea Vacondio
     *
     */
    static class SplitByEveryXPagesParametersBuilder extends SplitParametersBuilder<SplitByEveryXPagesParameters> {

        private final int step;

        SplitByEveryXPagesParametersBuilder(int step) {
            this.step = step;
        }

        @Override
        public SplitByEveryXPagesParameters build() {
            SplitByEveryXPagesParameters params = new SplitByEveryXPagesParameters(step);
            params.setCompress(isCompress());
            params.setExistingOutputPolicy(existingOutput());
            params.setVersion(getVersion());
            params.setOutput(getOutput());
            params.setOutputPrefix(getPrefix());
            params.addSource(getSource());
            params.setOptimizationPolicy(getOptimizationPolicy());
            params.discardOutline(isDiscardBookmarks());
            return params;
        }
    }

}
