/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/mag/2014
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
package org.pdfsam.ui.info;

import javafx.geometry.Side;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.context.DefaultI18nContext;

/**
 * Panel showing a pdf document information like author, creator etc.
 * 
 * @author Andrea Vacondio
 *
 */
@Named
class InfoPane extends TabPane {

    @Inject
    private SummaryTab summary;
    private Tab security = createTab(DefaultI18nContext.getInstance().i18n("Security"));
    @Inject
    private KeywordsTab keywords;

    @PostConstruct
    void init() {
        setSide(Side.LEFT);
        getTabs().addAll(summary, security, keywords);
    }

    private static Tab createTab(String title) {
        Tab tab = new Tab(title);
        tab.setClosable(false);
        return tab;
    }
}
