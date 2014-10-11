/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14/giu/2014
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
package org.pdfsam.splitbysize;

import java.math.BigDecimal;
import java.util.function.Consumer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javafx.beans.property.ReadOnlyObjectProperty;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Tooltip;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.support.params.TaskParametersBuildStep;
import org.pdfsam.support.validation.Validators;
import org.pdfsam.ui.support.FXValidationSupport;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.pdfsam.ui.support.Style;

/**
 * Combo box letting the user specify the filesize in the split by size task
 * 
 * @author Andrea Vacondio
 *
 */
class SizeComboBox extends ComboBox<String> implements TaskParametersBuildStep<SplitBySizeParametersBuilder> {
    private static final String REGEXP = "(?i)^(\\d+)(\\s*)([KB||MB]*)";
    private static final Pattern PATTERN = Pattern.compile(REGEXP);

    private final FXValidationSupport<String> validationSupport = new FXValidationSupport<>();

    SizeComboBox() {
        getItems().addAll("500 KB", "1 MB", "2 MB", "3 MB", "4 MB", "5 MB", "10 MB");

        validationSupport.setValidator(Validators.newRegexMatchingString(REGEXP));
        setEditable(true);
        getSelectionModel().selectFirst();
        valueProperty().addListener((o, oldVal, newVal) -> validate());
        setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n(
                "Set the size as number of bytes or specify KB or MB")));
        validationSupport.validationStateProperty().addListener(o -> {
            if (validationSupport.validationStateProperty().get() == ValidationState.INVALID) {
                getEditor().getStyleClass().addAll(Style.INVALID.css());
            } else {
                getEditor().getStyleClass().removeAll(Style.INVALID.css());
            }
        });
    }

    public final ValidationState getValidationState() {
        return validationSupport.validationStateProperty().get();
    }

    public final ReadOnlyObjectProperty<ValidationState> validProperty() {
        return validationSupport.validationStateProperty();
    }

    /**
     * Triggers a validation programmatically
     */
    public void validate() {
        validationSupport.validate(getSelectionModel().getSelectedItem());
    }

    public void apply(SplitBySizeParametersBuilder builder, Consumer<String> onError) {
        this.validate();
        if (validationSupport.validationStateProperty().get() == ValidationState.VALID) {
            Matcher matcher = PATTERN.matcher(getSelectionModel().getSelectedItem());
            if (matcher.matches()) {
                BigDecimal value = new BigDecimal(matcher.group(1));
                String unit = matcher.group(3);
                if ("KB".equalsIgnoreCase(unit)) {
                    value = value.multiply(new BigDecimal(1024));
                } else if ("MB".equalsIgnoreCase(unit)) {
                    value = value.multiply(new BigDecimal(1024 * 1024));
                }
                builder.size(value.longValue());
            }
        } else {
            onError.accept(DefaultI18nContext.getInstance().i18n("Invalid split size"));
        }
    }
}
