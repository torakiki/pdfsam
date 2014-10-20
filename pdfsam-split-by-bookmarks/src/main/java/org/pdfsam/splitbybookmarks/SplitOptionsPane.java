/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 08/apr/2014
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

import static org.apache.commons.lang3.StringUtils.isNotBlank;

import java.util.function.Consumer;

import javafx.scene.Node;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.support.params.TaskParametersBuildStep;
import org.pdfsam.ui.support.Style;

/**
 * Panel for the Split options
 * 
 * @author Andrea Vacondio
 *
 */
class SplitOptionsPane extends VBox implements TaskParametersBuildStep<SplitByGoToActionLevelParametersBuilder> {

    private BookmarksLevelComboBox levelCombo = new BookmarksLevelComboBox();
    private TextField regexpField = new TextField();

    SplitOptionsPane() {
        super(Style.DEFAULT_SPACING);
        getStyleClass().addAll(Style.CONTAINER.css());
        levelCombo.setId("bookmarksLevel");
        regexpField.setId("bookmarksRegexp");
        regexpField.setPromptText(DefaultI18nContext.getInstance().i18n("Regexp the bookmark has to match"));
        regexpField.setPrefWidth(300);
        getChildren().addAll(
                createLine(new Label(DefaultI18nContext.getInstance().i18n("Split at this bookmark level:")),
                        levelCombo),
                createLine(new Label(DefaultI18nContext.getInstance().i18n("Matching regular expression:")),
                        regexpField));
    }

    void setMaxBookmarkLevel(int max) {
        levelCombo.setMaxBookmarkLevel(max);
    }

    private HBox createLine(Node... items) {
        HBox item = new HBox(items);
        item.getStyleClass().addAll(Style.VITEM.css());
        item.getStyleClass().addAll(Style.HCONTAINER.css());
        return item;
    }

    public void apply(SplitByGoToActionLevelParametersBuilder builder, Consumer<String> onError) {
        levelCombo.apply(builder, onError);
        if (isNotBlank(regexpField.getText())) {
            builder.regexp(regexpField.getText());
        }
    }
}
