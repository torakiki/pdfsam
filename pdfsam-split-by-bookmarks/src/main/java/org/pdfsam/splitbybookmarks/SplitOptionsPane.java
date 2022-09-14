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
package org.pdfsam.splitbybookmarks;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.pdfsam.ui.help.HelpUtils.helpIcon;

import java.util.Map;
import java.util.Optional;
import java.util.SortedSet;
import java.util.function.Consumer;

import org.pdfsam.i18n.I18nContext;
import org.pdfsam.support.params.TaskParametersBuildStep;
import org.pdfsam.ui.ResettableView;
import org.pdfsam.ui.support.Style;
import org.pdfsam.ui.workspace.RestorableView;

import javafx.scene.Node;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.text.Text;
import javafx.scene.text.TextFlow;

/**
 * Panel for the Split options
 * 
 * @author Andrea Vacondio
 *
 */
class SplitOptionsPane extends VBox
        implements TaskParametersBuildStep<SplitByOutlineLevelParametersBuilder>, RestorableView, ResettableView {

    private BookmarksLevelComboBox levelCombo = new BookmarksLevelComboBox();
    private TextField regexpField = new TextField();

    SplitOptionsPane() {
        super(Style.DEFAULT_SPACING);
        getStyleClass().addAll(Style.CONTAINER.css());
        I18nContext ctx = I18nContext.getInstance();
        levelCombo.setId("bookmarksLevel");
        regexpField.setId("bookmarksRegexp");
        regexpField.setPromptText(ctx.i18n("Regular expression the bookmark has to match"));
        regexpField.setPrefWidth(350);
        getChildren().addAll(createLine(new Label(ctx.i18n("Split at this bookmark level:")), levelCombo),
                createLine(new Label(ctx.i18n("Matching regular expression:")), regexpField, helpIcon(new TextFlow(
                        new Text(ctx.i18n("A regular expression the bookmark text has to match")
                                + System.lineSeparator()),
                        new Text(ctx.i18n(
                                "Example: use .*Chapter.* to match bookmarks containing the word \"Chapter\""))))));
    }

    void setValidBookmarkLevels(SortedSet<Integer> levels) {
        levelCombo.setValidBookmarkLevels(levels);
    }

    private HBox createLine(Node... items) {
        HBox item = new HBox(items);
        item.getStyleClass().addAll(Style.VITEM.css());
        item.getStyleClass().addAll(Style.HCONTAINER.css());
        return item;
    }

    @Override
    public void resetView() {
        regexpField.clear();
        levelCombo.resetView();
    }

    @Override
    public void apply(SplitByOutlineLevelParametersBuilder builder, Consumer<String> onError) {
        levelCombo.apply(builder, onError);
        if (isNotBlank(regexpField.getText())) {
            builder.regexp(regexpField.getText());
        }
    }

    @Override
    public void saveStateTo(Map<String, String> data) {
        data.put("regexp", defaultString(regexpField.getText()));
        levelCombo.saveStateTo(data);
    }

    @Override
    public void restoreStateFrom(Map<String, String> data) {
        regexpField.setText(Optional.ofNullable(data.get("regexp")).orElse(EMPTY));
        levelCombo.restoreStateFrom(data);
    }
}
