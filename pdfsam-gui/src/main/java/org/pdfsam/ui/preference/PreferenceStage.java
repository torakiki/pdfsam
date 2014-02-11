/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 29/ott/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.ui.preference;

import java.util.Collection;
import java.util.List;

import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.scene.layout.BorderPane;
import javafx.stage.Stage;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;
import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.ui.ClosePane;
import org.pdfsam.ui.support.Style;

/**
 * Stage holding all the application preferences that the user can set.
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
public class PreferenceStage extends Stage {
    @Inject
    private PreferencePane preferencePane;
    @Resource(name = "styles")
    private List<String> styles;
    @Inject
    private Collection<Image> logos;

    @PostConstruct
    void init() {
        BorderPane containerPane = new BorderPane();
        containerPane.getStyleClass().addAll(Style.CONTAINER.css());
        containerPane.setCenter(preferencePane);
        containerPane.setBottom(new ClosePane());
        Scene scene = new Scene(containerPane);
        scene.getStylesheets().addAll(styles);
        setScene(scene);
        getIcons().addAll(logos);
        setTitle(DefaultI18nContext.getInstance().i18n("Preferences"));
    }
}
