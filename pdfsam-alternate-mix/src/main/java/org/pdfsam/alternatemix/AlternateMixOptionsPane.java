/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 26/giu/2014
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
package org.pdfsam.alternatemix;

import java.util.function.Consumer;

import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.support.params.TaskParametersBuildStep;
import org.pdfsam.support.validation.Validators;
import org.pdfsam.ui.commons.ValidableTextField;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.pdfsam.ui.support.Style;

/**
 * Panel for the Alternate Mix options
 * 
 * @author Andrea Vacondio
 *
 */
class AlternateMixOptionsPane extends VBox implements TaskParametersBuildStep<AlternateMixParametersBuilder> {

    private CheckBox reverseFirst = new CheckBox(DefaultI18nContext.getInstance().i18n("Reverse the first document"));
    private CheckBox reverseSecond = new CheckBox(DefaultI18nContext.getInstance().i18n("Reverse the second document"));
    private ValidableTextField firstStep = createValidableField();
    private ValidableTextField secondStep = createValidableField();

    AlternateMixOptionsPane() {
        super(Style.DEFAULT_SPACING);
        this.reverseSecond.setSelected(true);
        getStyleClass().addAll(Style.CONTAINER.css());
        HBox firstStepContainer = new HBox(new Label(DefaultI18nContext.getInstance().i18n(
                "Switch from the first document to the second one after the following pages")), firstStep);
        firstStepContainer.getStyleClass().addAll(Style.HCONTAINER.css());
        HBox secondStepContainer = new HBox(new Label(DefaultI18nContext.getInstance().i18n(
                "Switch from the second document to the first one after the following pages")), secondStep);
        secondStepContainer.getStyleClass().addAll(Style.HCONTAINER.css());
        getChildren().addAll(this.reverseFirst, this.reverseSecond, firstStepContainer, secondStepContainer);
    }

    public void apply(AlternateMixParametersBuilder builder, Consumer<String> onError) {
        builder.reverseFirst(reverseFirst.isSelected());
        builder.reverseSecond(reverseSecond.isSelected());
        firstStep.validate();
        secondStep.validate();
        if (firstStep.getValidationState() == ValidationState.VALID
                && secondStep.getValidationState() == ValidationState.VALID) {
            builder.stepFirst(Integer.parseInt(firstStep.getText()));
            builder.stepSecond(Integer.parseInt(secondStep.getText()));
        } else {
            onError.accept(DefaultI18nContext.getInstance().i18n(
                    "Invalid parameter 'first and second step' must be numbers"));
        }
    }

    private static ValidableTextField createValidableField() {
        ValidableTextField field = new ValidableTextField();
        field.setEnableInvalidStyle(true);
        field.setErrorMessage(DefaultI18nContext.getInstance().i18n("Select the number of pages"));
        field.setValidator(Validators.newIntegerString());
        field.setOnEnterValidation(true);
        field.setText("1");
        field.setPrefWidth(50);
        return field;
    }
}
