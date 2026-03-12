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
package org.pdfsam.tools.extract;

import javafx.geometry.HPos;
import javafx.geometry.VPos;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.layout.GridPane;
import org.pdfsam.core.support.params.TaskParametersBuildStep;
import org.pdfsam.model.ui.ResettableView;
import org.pdfsam.model.ui.workspace.RestorableView;
import org.pdfsam.ui.components.commons.ValidableTextField;
import org.pdfsam.ui.components.support.FXValidationSupport.ValidationState;
import org.pdfsam.ui.components.support.Style;
import org.sejda.conversion.exception.ConversionException;

import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.pdfsam.core.support.params.ConversionUtils.toPagesSelectionSet;
import static org.pdfsam.i18n.I18nContext.i18n;
import static org.pdfsam.ui.components.support.Views.helpIcon;

/**
 * Panel for the Extract options
 *
 * @author Andrea Vacondio
 */
class ExtractOptionsPane extends GridPane
        implements TaskParametersBuildStep<ExtractParametersBuilder>, RestorableView, ResettableView {

    private final ValidableTextField field = new ValidableTextField();
    private final CheckBox separateFile;

    ExtractOptionsPane() {
        var separateFileHelp = i18n().tr("Each continuous series of pages will generate a separate PDF file");
        this.separateFile = new CheckBox(i18n().tr("A separate file for each set of pages"));
        this.separateFile.setGraphic(helpIcon(separateFileHelp));
        this.separateFile.setAccessibleHelp(separateFileHelp);
        this.separateFile.getStyleClass().addAll(Style.WITH_HELP.css());
        this.separateFile.setId("separateFile");

        this.field.setOnEnterValidation(true);
        this.field.setEnableInvalidStyle(true);
        this.field.setPromptText(i18n().tr("Pages to extract (ex: 2 or 5-23 or 2,5-7,12- or 3,last)"));
        this.field.setValidator(v -> {
            try {
                return !toPagesSelectionSet(this.field.getText()).isEmpty();
            } catch (ConversionException e) {
                return false;
            }
        });
        this.field.setErrorMessage(i18n().tr("Invalid page ranges"));
        this.field.setId("extractRanges");
        this.field.setPrefWidth(400);
        var fieldHelp = i18n().tr(
                "Comma separated page numbers or ranges to extract (ex: 2 or 5-23 or 2,5-7,12- or 3,last)");
        this.field.setAccessibleHelp(fieldHelp);

        getStyleClass().addAll(Style.CONTAINER.css());
        getStyleClass().addAll(Style.GRID.css());
        var label = new Label(i18n().tr("Extract pages:"));
        label.setLabelFor(field);
        GridPane.setValignment(label, VPos.BOTTOM);
        GridPane.setHalignment(label, HPos.LEFT);
        add(label, 0, 0);
        GridPane.setValignment(field, VPos.BOTTOM);
        GridPane.setHalignment(field, HPos.LEFT);
        add(field, 1, 0);
        var helpIcon = helpIcon(fieldHelp);
        GridPane.setValignment(helpIcon, VPos.CENTER);
        add(helpIcon, 2, 0);
        GridPane.setValignment(separateFile, VPos.BOTTOM);
        GridPane.setHalignment(separateFile, HPos.LEFT);
        add(separateFile, 0, 1, 3, 1);
    }

    @Override
    public void apply(ExtractParametersBuilder builder, Consumer<String> onError) {
        this.field.validate();
        if (this.field.getValidationState() == ValidationState.VALID) {
            try {
                builder.pagesSelection(toPagesSelectionSet(this.field.getText()));
                builder.separateForEachRange(separateFile.isSelected());
            } catch (ConversionException e) {
                onError.accept(e.getMessage());
            }
        } else {
            onError.accept(i18n().tr("Invalid page ranges"));
        }
    }

    @Override
    public void saveStateTo(Map<String, String> data) {
        data.put("pages", defaultString(field.getText()));
        data.put("separateFile", Boolean.toString(separateFile.isSelected()));
    }

    @Override
    public void restoreStateFrom(Map<String, String> data) {
        field.setText(Optional.ofNullable(data.get("pages")).orElse(EMPTY));
        separateFile.setSelected(Boolean.parseBoolean(data.get("separateFile")));
    }

    @Override
    public void resetView() {
        this.field.clear();
        separateFile.setSelected(false);
    }
}
