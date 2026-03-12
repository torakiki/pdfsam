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
package org.pdfsam.tools.rotate;

import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import org.pdfsam.core.support.params.TaskParametersBuildStep;
import org.pdfsam.model.ui.ComboItem;
import org.pdfsam.model.ui.ResettableView;
import org.pdfsam.model.ui.workspace.RestorableView;
import org.pdfsam.ui.components.support.Style;
import org.sejda.model.pdf.page.PredefinedSetOfPages;
import org.sejda.model.rotation.Rotation;

import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.pdfsam.i18n.I18nContext.i18n;

/**
 * Panel for the Rotate options
 * 
 * @author Andrea Vacondio
 *
 */
class RotateOptionsPane extends HBox
        implements TaskParametersBuildStep<RotateParametersBuilder>, RestorableView, ResettableView {

    private final ComboBox<ComboItem<PredefinedSetOfPages>> rotationType = new ComboBox<>();
    private final ComboBox<ComboItem<Rotation>> rotation = new ComboBox<>();

    RotateOptionsPane() {
        super(Style.DEFAULT_SPACING);
        this.rotationType.getItems().add(new ComboItem<>(PredefinedSetOfPages.ALL_PAGES, i18n().tr("All pages")));
        this.rotationType.getItems().add(new ComboItem<>(PredefinedSetOfPages.EVEN_PAGES, i18n().tr("Even pages")));
        this.rotationType.getItems().add(new ComboItem<>(PredefinedSetOfPages.ODD_PAGES, i18n().tr("Odd pages")));
        this.rotationType.setId("rotationType");

        this.rotation.getItems().add(new ComboItem<>(Rotation.DEGREES_90, i18n().tr("90 degrees clockwise")));
        this.rotation.getItems().add(new ComboItem<>(Rotation.DEGREES_180, i18n().tr("180 degrees clockwise")));
        this.rotation.getItems().add(new ComboItem<>(Rotation.DEGREES_270, i18n().tr("90 degrees counterclockwise")));
        this.rotation.setId("rotation");

        getStyleClass().addAll(Style.HCONTAINER.css());
        getStyleClass().addAll(Style.CONTAINER.css());
        resetView();

        var rotateLabel = new Label(i18n().tr("Rotate") + " ");
        rotateLabel.setLabelFor(this.rotationType);
        this.rotationType.setAccessibleText(i18n().tr("Page selection for rotation"));
        this.rotationType.setAccessibleHelp(i18n().tr("Choose which pages to rotate: all, even, or odd"));
        this.rotation.setAccessibleText(i18n().tr("Rotation angle"));
        this.rotation.setAccessibleHelp(i18n().tr("Choose the rotation angle to apply"));
        getChildren().addAll(rotateLabel, this.rotationType, this.rotation);
    }

    @Override
    public void resetView() {
        this.rotationType.getSelectionModel().selectFirst();
        this.rotation.getSelectionModel().selectFirst();
    }

    @Override
    public void apply(RotateParametersBuilder builder, Consumer<String> onError) {
        builder.rotation(rotation.getSelectionModel().getSelectedItem().key());
        builder.rotationType(rotationType.getSelectionModel().getSelectedItem().key());
    }

    @Override
    public void saveStateTo(Map<String, String> data) {
        data.put("rotation", Optional.ofNullable(rotation.getSelectionModel().getSelectedItem())
                .map(i -> i.key().toString()).orElse(EMPTY));
        data.put("rotationType", Optional.ofNullable(rotationType.getSelectionModel().getSelectedItem())
                .map(i -> i.key().toString()).orElse(EMPTY));
    }

    @Override
    public void restoreStateFrom(Map<String, String> data) {
        Optional.ofNullable(data.get("rotation")).map(Rotation::valueOf)
                .flatMap(key -> this.rotation.getItems().stream().filter(i -> i.key().equals(key)).findFirst())
                .ifPresent(this.rotation.getSelectionModel()::select);
        Optional.ofNullable(data.get("rotationType")).map(PredefinedSetOfPages::valueOf)
                .flatMap(key -> this.rotationType.getItems().stream().filter(i -> i.key().equals(key)).findFirst())
                .ifPresent(this.rotationType.getSelectionModel()::select);
    }
}
