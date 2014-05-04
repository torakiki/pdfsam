/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 02/mag/2014
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
package org.pdfsam.ui.dashboard;

import java.io.IOException;

import javafx.fxml.FXMLLoader;
import javafx.scene.Group;
import javafx.scene.Node;
import javafx.scene.layout.Pane;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.context.DefaultI18nContext;
import org.springframework.core.io.ClassPathResource;

/**
 * About item for the dashboard
 * 
 * @author Andrea Vacondio
 *
 */
@Named
class AboutDashboadItem implements DashboardItem {

    @Inject
    private AboutPane pane;

    public String id() {
        return "ABOUT";
    }

    public String name() {
        return DefaultI18nContext.getInstance().i18n("About");
    }

    public Pane pane() {
        return pane;
    }

    public Node graphic() {
        try {
            return (Group) FXMLLoader.load(new ClassPathResource("/fxml/TestModule2.fxml").getURL());
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }

    public int priority() {
        return 0;
    }

}
