/*
 * This file is part of the PDF Split And Merge source code
 * Created on 08/apr/2014
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
package org.pdfsam.tools.splitbysize;

import javafx.scene.control.Label;
import javafx.scene.control.ToggleGroup;
import javafx.scene.layout.HBox;
import org.pdfsam.core.support.params.TaskParametersBuildStep;
import org.pdfsam.core.support.validation.Validators;
import org.pdfsam.model.ui.ResettableView;
import org.pdfsam.model.ui.workspace.RestorableView;
import org.pdfsam.ui.components.commons.ValidableTextField;
import org.pdfsam.ui.components.support.FXValidationSupport.ValidationState;
import org.pdfsam.ui.components.support.Style;

import java.util.Arrays;
import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Panel for the Split options
 *
 * @author Andrea Vacondio
 *
 */
class SplitOptionsPane extends HBox
        implements TaskParametersBuildStep<SplitBySizeParametersBuilder>, RestorableView, ResettableView {

    private final ValidableTextField field = new ValidableTextField();
    private final ToggleGroup group = new ToggleGroup();

    SplitOptionsPane() {
        this.field.setOnEnterValidation(true);
        this.field.setEnableInvalidStyle(true);
        this.field.setPromptText(i18n().tr("Set the size to split at"));
        this.field.setValidator(Validators.positiveInteger());
        this.field.setErrorMessage(i18n().tr("Size must be a number"));
        this.field.setId("sizeField");
        this.field.setAccessibleText(i18n().tr("Split at this size"));
        this.field.setAccessibleHelp(i18n().tr("Enter a positive integer for the maximum file size"));
        getStyleClass().addAll(Style.CONTAINER.css());
        getStyleClass().addAll(Style.HCONTAINER.css());
        var sizeLabel = new Label(i18n().tr("Split at this size") + ":");
        sizeLabel.setLabelFor(this.field);
        getChildren().addAll(sizeLabel, this.field);
        Arrays.stream(SizeUnit.values()).map(SizeUnitRadio::new).forEach(r -> {
            r.setToggleGroup(group);
            getChildren().add(r);
        });
        group.getToggles().stream().findFirst().ifPresent(t -> t.setSelected(true));
    }

    @Override
    public void apply(SplitBySizeParametersBuilder builder, Consumer<String> onError) {
        this.field.validate();
        if (this.field.getValidationState() == ValidationState.VALID) {
            builder.size(
                    ((SizeUnitRadio) group.getSelectedToggle()).unit().toBytes(Integer.parseInt(this.field.getText())));
        } else {
            onError.accept(i18n().tr("Invalid split size"));
        }
    }

    @Override
    public void saveStateTo(Map<String, String> data) {
        data.put("size", defaultString(field.getText()));
        group.getToggles().stream().map(t -> (SizeUnitRadio) t).forEach(s -> s.saveStateTo(data));
    }

    @Override
    public void restoreStateFrom(Map<String, String> data) {
        field.setText(Optional.ofNullable(data.get("size")).orElse(EMPTY));
        group.getToggles().stream().map(t -> (SizeUnitRadio) t).forEach(s -> s.restoreStateFrom(data));
    }

    @Override
    public void resetView() {
        field.clear();
        group.getToggles().stream().findFirst().ifPresent(t -> t.setSelected(true));
    }
}
