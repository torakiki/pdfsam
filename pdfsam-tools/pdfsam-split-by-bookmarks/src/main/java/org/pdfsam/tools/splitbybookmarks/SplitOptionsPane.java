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
package org.pdfsam.tools.splitbybookmarks;

import javafx.geometry.HPos;
import javafx.geometry.VPos;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.GridPane;
import org.pdfsam.core.support.params.TaskParametersBuildStep;
import org.pdfsam.model.ui.ResettableView;
import org.pdfsam.model.ui.workspace.RestorableView;
import org.pdfsam.ui.components.support.Style;

import java.util.Map;
import java.util.Optional;
import java.util.SortedSet;
import java.util.function.Consumer;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.pdfsam.i18n.I18nContext.i18n;
import static org.pdfsam.ui.components.support.Views.helpIcon;

/**
 * Panel for the Split options
 *
 * @author Andrea Vacondio
 */
class SplitOptionsPane extends GridPane
        implements TaskParametersBuildStep<SplitByOutlineLevelParametersBuilder>, RestorableView, ResettableView {

    private final BookmarksLevelComboBox levelCombo = new BookmarksLevelComboBox();
    private final TextField regexpField = new TextField();
    private final CheckBox hierarchicalOutputCheck = new CheckBox();
    private final TextField overlapPagesField = new TextField();

    SplitOptionsPane() {
        getStyleClass().addAll(Style.CONTAINER.css());
        getStyleClass().addAll(Style.GRID.css());
        levelCombo.setId("bookmarksLevel");
        regexpField.setId("bookmarksRegexp");
        regexpField.setPromptText(i18n().tr("Regular expression the bookmark has to match"));
        regexpField.setPrefWidth(350);
        var label = new Label(i18n().tr("Split at this bookmark level:"));
        GridPane.setValignment(label, VPos.BOTTOM);
        GridPane.setHalignment(label, HPos.LEFT);
        add(label, 0, 0);
        GridPane.setValignment(levelCombo, VPos.BOTTOM);
        GridPane.setHalignment(levelCombo, HPos.LEFT);
        add(levelCombo, 1, 0, 2, 1);
        var regexLabel = new Label(i18n().tr("Matching regular expression:"));
        GridPane.setValignment(regexLabel, VPos.BOTTOM);
        GridPane.setHalignment(regexLabel, HPos.LEFT);
        add(regexLabel, 0, 1);
        GridPane.setValignment(regexpField, VPos.BOTTOM);
        GridPane.setHalignment(regexpField, HPos.LEFT);
        add(regexpField, 1, 1);
        var helpIcon = helpIcon("""
                %s
                %s
                """.formatted(i18n().tr("A regular expression the bookmark text has to match"),
                i18n().tr("Example: use .*Chapter.* to match bookmarks containing the word \"Chapter\"")));
        GridPane.setValignment(helpIcon, VPos.CENTER);
        GridPane.setHalignment(helpIcon, HPos.LEFT);
        add(helpIcon, 2, 1);

        hierarchicalOutputCheck.setId("hierarchicalOutput");
        hierarchicalOutputCheck.setText(i18n().tr("Create hierarchical directory structure"));
        GridPane.setValignment(hierarchicalOutputCheck, VPos.CENTER);
        GridPane.setHalignment(hierarchicalOutputCheck, HPos.LEFT);
        add(hierarchicalOutputCheck, 0, 2, 3, 1);
        var hierarchicalHelpIcon = helpIcon("""
                %s
                %s
                """.formatted(i18n().tr("When enabled, creates a directory for each top-level bookmark and places split files inside"),
                i18n().tr("Example: Chapter_1/ contains 1_1.pdf, 1_2.pdf, etc.")));
        GridPane.setValignment(hierarchicalHelpIcon, VPos.CENTER);
        GridPane.setHalignment(hierarchicalHelpIcon, HPos.LEFT);
        add(hierarchicalHelpIcon, 3, 2);

        var overlapLabel = new Label(i18n().tr("Additional overlap pages:"));
        GridPane.setValignment(overlapLabel, VPos.BOTTOM);
        GridPane.setHalignment(overlapLabel, HPos.LEFT);
        add(overlapLabel, 0, 3);
        overlapPagesField.setId("overlapPages");
        overlapPagesField.setPromptText("0");
        overlapPagesField.setPrefWidth(100);
        GridPane.setValignment(overlapPagesField, VPos.BOTTOM);
        GridPane.setHalignment(overlapPagesField, HPos.LEFT);
        add(overlapPagesField, 1, 3);
        var overlapHelpIcon = helpIcon("""
                %s
                %s
                %s
                """.formatted(i18n().tr("Optional: additional pages to include beyond auto-detected overlaps"),
                i18n().tr("Auto-detection includes pages where sections naturally flow together"),
                i18n().tr("Example: use 1 or 2 for extra safety margin")));
        GridPane.setValignment(overlapHelpIcon, VPos.CENTER);
        GridPane.setHalignment(overlapHelpIcon, HPos.LEFT);
        add(overlapHelpIcon, 2, 3);
    }

    void setValidBookmarkLevels(SortedSet<Integer> levels) {
        levelCombo.setValidBookmarkLevels(levels);
    }

    @Override
    public void resetView() {
        regexpField.clear();
        levelCombo.resetView();
        hierarchicalOutputCheck.setSelected(false);
        overlapPagesField.clear();
    }

    @Override
    public void apply(SplitByOutlineLevelParametersBuilder builder, Consumer<String> onError) {
        levelCombo.apply(builder, onError);
        if (isNotBlank(regexpField.getText())) {
            builder.regexp(regexpField.getText());
        }
        builder.hierarchicalOutput(hierarchicalOutputCheck.isSelected());

        // Auto-detect is now always enabled when hierarchical output is selected
        builder.autoDetectOverlap(hierarchicalOutputCheck.isSelected());

        if (isNotBlank(overlapPagesField.getText())) {
            try {
                int overlap = Integer.parseInt(overlapPagesField.getText().trim());
                if (overlap >= 0) {
                    builder.overlapPages(overlap);
                } else {
                    onError.accept(i18n().tr("Overlap pages must be a positive number"));
                }
            } catch (NumberFormatException e) {
                onError.accept(i18n().tr("Invalid overlap pages value"));
            }
        }
    }

    @Override
    public void saveStateTo(Map<String, String> data) {
        data.put("regexp", defaultString(regexpField.getText()));
        data.put("hierarchicalOutput", Boolean.toString(hierarchicalOutputCheck.isSelected()));
        data.put("overlapPages", defaultString(overlapPagesField.getText()));
        levelCombo.saveStateTo(data);
    }

    @Override
    public void restoreStateFrom(Map<String, String> data) {
        regexpField.setText(Optional.ofNullable(data.get("regexp")).orElse(EMPTY));
        hierarchicalOutputCheck.setSelected(Boolean.parseBoolean(data.get("hierarchicalOutput")));
        overlapPagesField.setText(Optional.ofNullable(data.get("overlapPages")).orElse(EMPTY));
        levelCombo.restoreStateFrom(data);
    }
}
