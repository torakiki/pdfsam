/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 08/apr/2014
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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
package org.pdfsam.extract;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.pdfsam.support.params.ConversionUtils.toPageRangeSet;
import static org.pdfsam.ui.help.HelpUtils.helpIcon;

import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.support.params.ConversionUtils;
import org.pdfsam.support.params.TaskParametersBuildStep;
import org.pdfsam.ui.ResettableView;
import org.pdfsam.ui.commons.ValidableTextField;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.pdfsam.ui.support.Style;
import org.pdfsam.ui.workspace.RestorableView;
import org.sejda.conversion.exception.ConversionException;

import javafx.scene.control.Label;
import javafx.scene.layout.HBox;

/**
 * Panel for the Extract options
 * 
 * @author Andrea Vacondio
 *
 */
class ExtractOptionsPane extends HBox
        implements TaskParametersBuildStep<ExtractParametersBuilder>, RestorableView, ResettableView {

    private final ValidableTextField field = new ValidableTextField();

    ExtractOptionsPane() {
        this.field.setOnEnterValidation(true);
        this.field.setEnableInvalidStyle(true);
        this.field
                .setPromptText(DefaultI18nContext.getInstance().i18n("Pages to extract (ex: 2 or 5-23 or 2,5-7,12-)"));
        this.field.setValidator(v -> {
            try {
                return !toPageRangeSet(this.field.getText()).isEmpty();
            } catch (ConversionException e) {
                return false;
            }
        });
        this.field.setErrorMessage(DefaultI18nContext.getInstance().i18n("Invalid page ranges"));
        this.field.setId("extractRanges");
        this.field.setPrefWidth(350);
        getStyleClass().addAll(Style.CONTAINER.css());
        getStyleClass().addAll(Style.HCONTAINER.css());
        getChildren().addAll(new Label(DefaultI18nContext.getInstance().i18n("Extract pages:")), this.field,
                helpIcon("Comma separated page numbers or ranges to extract (ex: 2 or 5-23 or 2,5-7,12-)"));
    }

    @Override
    public void apply(ExtractParametersBuilder builder, Consumer<String> onError) {
        this.field.validate();
        if (this.field.getValidationState() == ValidationState.VALID) {
            try {
                builder.ranges(ConversionUtils.toPageRangeSet(this.field.getText()));
            } catch (ConversionException e) {
                onError.accept(e.getMessage());
            }
        } else {
            onError.accept(DefaultI18nContext.getInstance().i18n("Invalid page ranges"));
        }
    }

    @Override
    public void saveStateTo(Map<String, String> data) {
        data.put("pages", defaultString(field.getText()));
    }

    @Override
    public void restoreStateFrom(Map<String, String> data) {
        field.setText(Optional.ofNullable(data.get("pages")).orElse(EMPTY));
    }

    @Override
    public void resetView() {
        this.field.setText("");
    }
}
