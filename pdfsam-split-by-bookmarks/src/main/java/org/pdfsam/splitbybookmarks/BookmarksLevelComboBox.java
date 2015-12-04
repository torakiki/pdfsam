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
package org.pdfsam.splitbybookmarks;

import static java.util.Objects.nonNull;
import static java.util.Optional.ofNullable;

import java.util.Map;
import java.util.function.Consumer;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.support.params.TaskParametersBuildStep;
import org.pdfsam.support.validation.Validators;
import org.pdfsam.ui.support.FXValidationSupport;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.pdfsam.ui.support.Style;
import org.pdfsam.ui.workspace.RestorableView;

import javafx.beans.property.ReadOnlyObjectProperty;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Tooltip;

/**
 * Combo box letting the user specify the filesize in the split by size task
 * 
 * @author Andrea Vacondio
 *
 */
class BookmarksLevelComboBox extends ComboBox<String>
        implements TaskParametersBuildStep<SplitByOutlineLevelParametersBuilder>, RestorableView {
    private final FXValidationSupport<String> validationSupport = new FXValidationSupport<>();

    BookmarksLevelComboBox() {
        validationSupport.setValidator(Validators.alwaysFalse());
        setEditable(true);
        getSelectionModel().selectFirst();
        valueProperty().addListener((o, oldVal, newVal) -> validate());
        setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("Set the bookmarks level to split at")));
        validationSupport.validationStateProperty().addListener(o -> {
            if (validationSupport.validationStateProperty().get() == ValidationState.INVALID) {
                getEditor().getStyleClass().addAll(Style.INVALID.css());
            } else {
                getEditor().getStyleClass().removeAll(Style.INVALID.css());
            }
        });
        getEditor().focusedProperty().addListener((obs, old, isFocused) -> {
            // workaround for https://bugs.openjdk.java.net/browse/JDK-8136838
            if (!isFocused) {
                String newVal = getEditor().getText();
                if (nonNull(newVal) && !newVal.equals(getValue())) {
                    setValue(newVal);
                }
            }
        });
    }

    public void setMaxBookmarkLevel(int max) {
        getItems().clear();
        if (max > 0) {
            validationSupport.setValidator(Validators.positiveIntRange(1, max));
            for (int i = 1; i <= max; i++) {
                getItems().add(Integer.toString(i));
            }
        } else {
            validationSupport.setValidator(Validators.alwaysFalse());
        }
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
        validationSupport.validate(getValue());
    }

    public void apply(SplitByOutlineLevelParametersBuilder builder, Consumer<String> onError) {
        this.validate();
        if (validationSupport.validationStateProperty().get() == ValidationState.VALID) {
            builder.level(Integer.parseInt(getValue()));
        } else {
            onError.accept(DefaultI18nContext.getInstance().i18n("Invalid bookmarks level"));
        }
    }

    public void saveStateTo(Map<String, String> data) {
        data.put("levelCombo.max", Integer.toString(getItems().size()));
        data.put("levelCombo.selected", ofNullable(getValue()).orElse(""));
    }

    public void restoreStateFrom(Map<String, String> data) {
        getSelectionModel().selectFirst();
        ofNullable(data.get("levelCombo.max")).map(Integer::valueOf).ifPresent(this::setMaxBookmarkLevel);
        setValue(ofNullable(data.get("levelCombo.selected")).orElse(""));
    }
}
