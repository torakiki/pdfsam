/*
 * This file is part of the PDF Split And Merge source code
 * Created on 09/giu/2014
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
import org.sejda.conversion.PageNumbersListAdapter;
import org.sejda.model.parameter.SplitByPagesParameters;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * {@link RadioButton} driving a text field that can accept a comma separated list of integer numbers.
 * 
 * @author Andrea Vacondio
 *
 */
class SplitAfterRadioButton extends RadioButton
        implements SplitParametersBuilderCreator, RestorableView, ResettableView {

    private final ValidableTextField field;

    public SplitAfterRadioButton(ValidableTextField field) {
        super(i18n().tr("Split after the following page numbers"));
        this.setAccessibleHelp(i18n().tr("Select to split the document after specific page numbers"));
        this.field = field;
        this.field.setOnEnterValidation(true);
        this.field.setEnableInvalidStyle(true);
        this.field.setPromptText(i18n().tr("Page numbers to split at (n1,n2,n3..)"));
        this.field.setValidator(Validators.regexMatching("^([1-9]\\d*(\\s*,\\s*)?)+$"));
        this.field.setErrorMessage(i18n().tr("Invalid page numbers"));
        this.field.setAccessibleText(i18n().tr("Page numbers to split at"));
        this.field.setAccessibleHelp(i18n().tr("Enter comma separated positive page numbers (Ex. 1,5,12...)"));
    }

    @Override
    public SplitByPageParametersBuilder getBuilder(Consumer<String> onError) {
        this.field.validate();
        if (this.field.getValidationState() == ValidationState.VALID) {
            return new SplitByPageParametersBuilder(new PageNumbersListAdapter(this.field.getText()).getPageNumbers());
        }
        onError.accept(i18n().tr("Only valid positive page numbers are allowed"));
        return null;
    }

    @Override
    public void saveStateTo(Map<String, String> data) {
        if (isSelected()) {
            data.put("splitAfter", Boolean.TRUE.toString());
        }
        data.put("splitAfter.field", defaultString(field.getText()));
    }

    @Override
    public void restoreStateFrom(Map<String, String> data) {
        Optional.ofNullable(data.get("splitAfter")).map(Boolean::valueOf).ifPresent(this::setSelected);
        field.setText(Optional.ofNullable(data.get("splitAfter.field")).orElse(EMPTY));
    }

    @Override
    public void resetView() {
        field.clear();
    }

    /**
     * Builder for the {@link SplitByPagesParameters}
     * 
     * @author Andrea Vacondio
     *
     */
    static class SplitByPageParametersBuilder extends SplitParametersBuilder<SplitByPagesParameters> {

        private final List<Integer> pages;

        SplitByPageParametersBuilder(List<Integer> pages) {
            this.pages = pages;
        }

        @Override
        public SplitByPagesParameters build() {
            SplitByPagesParameters params = new SplitByPagesParameters();
            params.addPages(pages);
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
