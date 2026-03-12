/*
 * This file is part of the PDF Split And Merge source code
 * Created on 14/giu/2014
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
package org.pdfsam.tools.splitbybookmarks;

import javafx.beans.property.ReadOnlyObjectProperty;
import javafx.scene.control.ComboBox;
import org.pdfsam.core.support.params.TaskParametersBuildStep;
import org.pdfsam.core.support.validation.Validators;
import org.pdfsam.model.ui.ResettableView;
import org.pdfsam.model.ui.workspace.RestorableView;
import org.pdfsam.ui.components.support.FXValidationSupport;
import org.pdfsam.ui.components.support.FXValidationSupport.ValidationState;
import org.pdfsam.ui.components.support.Style;

import java.util.Arrays;
import java.util.Map;
import java.util.SortedSet;
import java.util.function.Consumer;
import java.util.stream.IntStream;

import static java.util.Objects.nonNull;
import static java.util.Optional.ofNullable;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Combo box letting the user select the bookmark level at which to split the document
 *
 * @author Andrea Vacondio
 */
class BookmarksLevelComboBox extends ComboBox<String>
        implements TaskParametersBuildStep<SplitByOutlineLevelParametersBuilder>, RestorableView, ResettableView {
    private final FXValidationSupport<String> validationSupport = new FXValidationSupport<>();

    BookmarksLevelComboBox() {
        setAccessibleText(i18n().tr("Bookmark level"));
        setAccessibleRoleDescription(i18n().tr("Bookmark level selector"));
        setPromptText(i18n().tr("Select a bookmark level"));
        validationSupport.setValidator(Validators.alwaysFalse());
        setEditable(true);
        getSelectionModel().selectFirst();
        valueProperty().addListener((o, oldVal, newVal) -> validate());
        validationSupport.validationStateProperty().addListener(o -> {
            if (validationSupport.validationStateProperty().get() == ValidationState.INVALID) {
                getEditor().getStyleClass().addAll(Style.INVALID.css());
                setAccessibleHelp(i18n().tr("Invalid bookmarks level"));
            } else {
                getEditor().getStyleClass().removeAll(Style.INVALID.css());
                setAccessibleHelp(null);
            }
        });
    }

    public void setValidBookmarkLevels(SortedSet<Integer> levels) {
        getItems().clear();
        if (nonNull(levels)) {
            validationSupport.setValidator(Validators.containedInteger(levels));
            levels.stream().map(Object::toString).forEach(getItems()::add);
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

    @Override
    public void resetView() {
        getItems().clear();
        getEditor().clear();
    }

    @Override
    public void apply(SplitByOutlineLevelParametersBuilder builder, Consumer<String> onError) {
        this.validate();
        if (validationSupport.validationStateProperty().get() == ValidationState.VALID) {
            builder.level(Integer.parseInt(getValue()));
        } else {
            onError.accept(i18n().tr("Invalid bookmarks level"));
        }
    }

    @Override
    public void saveStateTo(Map<String, String> data) {
        data.put("levelCombo.levels", String.join(",", getItems()));
        data.put("levelCombo.selected", ofNullable(getValue()).orElse(""));
    }

    @Override
    public void restoreStateFrom(Map<String, String> data) {
        getSelectionModel().selectFirst();
        ofNullable(data.get("levelCombo.max")).map(Integer::valueOf).ifPresent(max -> IntStream.rangeClosed(1, max).mapToObj(Integer::toString).forEach(getItems()::add));
        Arrays.stream(ofNullable(data.get("levelCombo.levels")).map(l -> l.split(",")).orElse(new String[0]))
                .forEach(getItems()::add);
        setValue(ofNullable(data.get("levelCombo.selected")).orElse(""));
    }
}
