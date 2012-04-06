/*
 * Created on 03/apr/2012
 * Copyright 2010 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation; 
 * either version 2 of the License.
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with this program; 
 * if not, write to the Free Software Foundation, Inc., 
 *  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package org.pdfsam.gui;

import javax.swing.Icon;

/**
 * A content panel
 * 
 * @author Andrea Vacondio
 * 
 */
public interface ContentPanel {

    /**
     * @return the id for this content panel
     */
    String getPanelId();

    /**
     * @return the name for the panel
     */
    String getPanelName();

    /**
     * @return the {@link Icon} for the panel
     */
    Icon getPanelIcon();
}
