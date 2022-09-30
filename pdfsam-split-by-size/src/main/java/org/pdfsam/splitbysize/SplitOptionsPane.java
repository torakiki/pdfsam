/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 08/apr/2014
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.defaultString;

import java.util.Arrays;
import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import org.pdfsam.core.support.params.TaskParametersBuildStep;
import org.pdfsam.core.support.validation.Validators;
import org.pdfsam.ui.ResettableView;
import org.pdfsam.ui.components.commons.ValidableTextField;
import org.pdfsam.ui.components.support.FXValidationSupport.ValidationState;
import org.pdfsam.ui.components.support.Style;
import org.pdfsam.ui.workspace.RestorableView;

import javafx.scene.control.Label;
import javafx.scene.control.ToggleGroup;
import javafx.scene.layout.HBox;

/**
 * Panel for the Split options
 * 
 * @author Andrea Vacondio
 *
 */
class SplitOptionsPane extends HBox
        implements TaskParametersBuildStep<SplitBySizeParametersBuilder>, RestorableView, ResettableView {

    private final ValidableTextField field = new ValidableTextField();
    private ToggleGroup group = new ToggleGroup();

    SplitOptionsPane() {
        this.field.setOnEnterValidation(true);
        this.field.setEnableInvalidStyle(true);
        this.field.setPromptText(i18n().tr("Set the size to split at"));
        this.field.setValidator(Validators.positiveInteger());
        this.field.setErrorMessage(i18n().tr("Size must be a number"));
        this.field.setId("sizeField");
        getStyleClass().addAll(Style.CONTAINER.css());
        getStyleClass().addAll(Style.HCONTAINER.css());
        getChildren().addAll(new Label(i18n().tr("Split at this size:")), this.field);
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
                    ((SizeUnitRadio) group.getSelectedToggle()).unit().toBytes(Integer.valueOf(this.field.getText())));
        } else {
            onError.accept(i18n().tr("Invalid split size"));
        }
    }

    @Override
    public void saveStateTo(Map<String, String> data) {
        data.put("size", defaultString(field.getText()));
        group.getToggles().stream().map(t -> {
            return (SizeUnitRadio) t;
        }).forEach(s -> s.saveStateTo(data));
    }

    @Override
    public void restoreStateFrom(Map<String, String> data) {
        field.setText(Optional.ofNullable(data.get("size")).orElse(EMPTY));
        group.getToggles().stream().map(t -> {
            return (SizeUnitRadio) t;
        }).forEach(s -> s.restoreStateFrom(data));
    }

    @Override
    public void resetView() {
        field.clear();
        group.getToggles().stream().findFirst().ifPresent(t -> t.setSelected(true));
    }
}
